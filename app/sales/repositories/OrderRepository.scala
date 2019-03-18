package sales.repositories

import java.util.UUID
import accounting.entities.InvoiceStatus
import cats.data.OptionT
import cats.effect.IO
import doobie._
import doobie.implicits._
import inventory.util.{SearchRequest, SearchResult}
import javax.inject.Inject
import sales.entities._
import shared.entities.LineItem
import utils.SolariusDB
import utils.imports.implicits._

final class OrderRepository @Inject()(solarius: SolariusDB) {

  def getById(orderId: Long): IO[Option[Order]] =
    solarius.run(Queries.getOrder("id", orderId.toString))

  def getById(orderId: UUID): IO[Option[Order]] =
    solarius.run(Queries.getOrder("uuid", orderId.toString))

  def getExpeditionDetails(orderId: Long): IO[Option[ExpeditionDetails]] =
    solarius.run(Queries.getExpeditionDetails(orderId))

  def search(sr: SearchRequest): IO[SearchResult[Order]] =
    solarius.run(Queries.search(sr))

  def getLineItems(orderId: Long): IO[List[LineItem]] =
    solarius.run(Queries.getLineItems(orderId))

  private object Queries {
    def getOrder(idType: String, id: String): ConnectionIO[Option[Order]] = {
      val where = idType match {
        case "uuid" => fr"url_id = $id"
        case _ => fr"id = $id"
      }

      val sql =
        fr"""
          SELECT
            o.id, o.sale_id, o.url_id,
            i.id AS invoiceId,
            i.status AS invoiceStatus,
            oa.value AS modelSku,
            u.full_name AS authorName,
            o.*,
            c.*
          FROM s_orders o
            JOIN customers c ON c.id = o.customer_id
            JOIN users u ON u.id = o.user_id
            LEFT JOIN s_order_attributes oa ON oa.order_id = o.id AND oa.name = "model_id"
            LEFT JOIN s_invoices i ON i.id = o.invoice_id
            WHERE """ ++ where

      sql
        .query[Order.OrderDB]
        .map(_.toEntity)
        .option
    }

    def getExpeditionDetails(orderId: Long): ConnectionIO[Option[ExpeditionDetails]] = {
      val sql =
        sql"""
        SELECT
          em.id,
          em.code,
          TRIM(CONCAT(oa.civic_number, " ", oa.street_name)) AS addressLine1,
          oa.unity AS addressLine2,
          c.name AS countryName,
          c.iso_code_2 AS countryCode,
          p.name AS stateName,
          oa.postal_code AS zipCode
        FROM s_orders AS o
          JOIN expedition_methods em ON em.id = o.expedition_method_id
          LEFT JOIN s_order_expedition_address oa ON oa.order_id = o.id
          LEFT JOIN countries c ON c.id = oa.country_id
          LEFT JOIN provinces p ON p.id = oa.province_id
        WHERE o.id = $orderId
        """

      val expeditionDetails = sql
        .query[(Long, String, Option[Address])]
        .option

      val attributesSql = sql"SELECT attribute_code, attribute_value FROM s_order_expedition_attributes AS oea WHERE order_id = $orderId"
      val expeditionAttributes = attributesSql
        .query[(String, String)]
        .to[List]
        .map(_.toMap)

      (for {
        (methodId, methodCode, address) <- OptionT(expeditionDetails)
        attributes <- OptionT.liftF(expeditionAttributes)
      } yield ExpeditionDetails(methodId, methodCode, address, attributes)).value
    }

    def search(sr: SearchRequest): ConnectionIO[SearchResult[Order]] = {
      val nonEmptyFilters = sr.filters.mapValues(_.trim).filterNot(_._2.isEmpty)
      val progressFilter: String => Fragment = _ match {
        case "cancelled" => fr"o.status = ${OrderStatus.fromString(OrderStatus.CANCELLED).get}"
        case "new" => fr"o.status = ${OrderStatus.fromString(OrderStatus.NEW).get}"
        case "manufactured" =>
          fr"o.status = ${OrderStatus.fromString(OrderStatus.APPROVED).get}" ++
            fr"o.progress_id = ${OrderDepartment.fromString(OrderDepartment.EXPEDITION).get}" ++
            fr"(i.id IS NULL OR i.status = ${InvoiceStatus.fromString(InvoiceStatus.NORMAL).get})"
        case "readyToShip" =>
          fr"o.status = ${OrderStatus.fromString(OrderStatus.APPROVED).get}" ++
            fr"o.progress_id = ${OrderDepartment.fromString(OrderDepartment.EXPEDITION).get}" ++
            fr"i.status = ${InvoiceStatus.fromString(InvoiceStatus.PAID).get}"
        case "shipped" =>
          fr"o.status = ${OrderStatus.fromString(OrderStatus.APPROVED).get}" ++
            fr"o.progress_id = ${OrderDepartment.fromString(OrderDepartment.COMPLETE).get}"
        case "inProgress" =>
          fr"o.status = ${OrderStatus.fromString(OrderStatus.APPROVED).get}" ++
            fr"${OrderDepartment.fromString(OrderDepartment.DISTRIBUTION).get} <= o.progress_id AND o.progress_id <= ${OrderDepartment.fromString(OrderDepartment.PRODUCTION).get}"
        case _ => fr"1 = 1"
      }
      val wheres: List[Option[Fragment]] = List(
        Some(fr"1 = 1"),
        nonEmptyFilters.get("name").map(value => fr"o.sale LIKE ${"%" + value + "%"}"),
        nonEmptyFilters.get("storeId").map(value => fr"o.branch_id = $value"),
        nonEmptyFilters.get("customerId").map(value => fr"c.id = $value"),
        nonEmptyFilters.get("customerName").map(value => fr"c.full_name LIKE ${"%" + value + "%"}"),
        nonEmptyFilters.get("department").flatMap(OrderDepartment.fromString).map(value => fr"o.progress_id = $value"),
        nonEmptyFilters.get("type").map(value => fr"o.type_id = $value"),
        nonEmptyFilters.get("status").map(value => fr"o.status = $value"),
        nonEmptyFilters.get("progress").map(progressFilter)
      )

      val allowedSortFields = Map(
        "id" -> "o.id",
        "name" -> "o.sale_id",
        "type" -> "o.type_id",
        "author" -> "o.user_id",
        "store" -> "o.branch_id",
        "createdAt" -> "o.creation_date",
        "updatedAt" -> "o.modification_date",
        "customerName" -> "c.full_name",
        "department" -> "o.progress_id",
        "status" -> "o.status"
      )

      val (sortField, sortOrder) = sr.sortField
        .flatMap(allowedSortFields.get)
        .map(field => (field, sr.sortOrder))
        .getOrElse(("o.creation_date", SearchRequest.SORT_ASC))

      val limitClause = sr.limit
        .map(lim => fr"LIMIT ${sr.offset}, $lim")
        .getOrElse(fr"LIMIT 100")

      val sql =
        fr"""
              SELECT
                o.id, o.url_id, o.sale_id, o.type_id, o.progress_id, o.status, o.branch_id,
                u.id, u.full_name, c.id, c.full_name, o.notes, o.tag, o.creation_date, o.modification_date,
                o.cancel_note, o.expedition_expedited_date, i.id, i.status, oa.value
              FROM s_orders o
                JOIN customers c ON c.id = o.customer_id
                JOIN users u ON u.id = o.user_id
                LEFT JOIN s_order_attributes oa ON oa.order_id = o.id AND oa.name = "model_id"
                LEFT JOIN s_invoices i ON i.id = o.invoice_id""" ++
          Fragments.whereAndOpt(wheres: _*) ++
          Fragment.const(s"ORDER BY $sortField $sortOrder") ++
          limitClause

      val fetchOrders: ConnectionIO[List[Order]] = sql
        .query[Order.OrderDB]
        .map(_.toEntity)
        .to[List]

      for {
        orders <- fetchOrders // Fetch all orders
        count <- sql"""SELECT FOUND_ROWS()""".query[Int].unique // Get the total number of records
      } yield SearchResult(orders, count)
    }

    def getLineItems(orderId: Long): ConnectionIO[List[LineItem]] = {
      val sql = sql"SELECT id, quantity, retail_price, sale_price, status, product_id, product_label FROM s_order_products WHERE order_id = $orderId"

      val getItems: ConnectionIO[List[LineItem]] = sql
        .query[LineItem.LineItemDB]
        .map(_.toEntity)
        .to[List]

      for {
        items <- getItems // Get the line items
        attributeOverrides <- getLineItemsAttributeOverrides(orderId) // Get all the attribute overrides
      } yield {
        // Merge line items with their attribute overrides
        items.map { item =>
          item.copy(attributeOverrides = attributeOverrides.getOrElse(item.id, List()))
        }
      }
    }

    private def getLineItemsAttributeOverrides(orderId: Long): ConnectionIO[Map[Long, Seq[(String, String)]]] = {
      val sql =
        sql"""
          SELECT *
          FROM s_order_product_attributes AS ipa
          JOIN s_order_products ip ON ip.id = ipa.product_record_id
          WHERE ip.order_id = $orderId
        """

      sql
        .query[(Long, String, String)]
        .to[List]
        .map { ls =>
          ls
            .groupBy(_._1)
            .mapValues(_.map(tuple => (tuple._2, tuple._3)))
        }
    }
  }

}
