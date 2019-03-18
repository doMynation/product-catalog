package accounting.repositories

import java.util.UUID
import accounting.entities.Invoice.InvoiceDB
import javax.inject.Inject
import accounting.entities._
import cats.implicits._
import cats.effect.IO
import inventory.util.{SearchRequest, SearchResult}
import shared._
import shared.entities.{ApplicableTaxes, LineItem, TaxComponent}
import utils.{SolariusDB}
import doobie._
import doobie.implicits._
import utils.imports.implicits._

final class InvoiceRepository @Inject()(solarius: SolariusDB) {
  implicit val han = LogHandler.jdkLogHandler

  def getById(invoiceId: Long): IO[Option[Invoice]] =
    solarius.run(Queries.getInvoice("id", invoiceId.toString))

  def getById(invoiceId: UUID): IO[Option[Invoice]] =
    solarius.run(Queries.getInvoice("uuid", invoiceId.toString))

  def getInvoiceLineItems(invoiceId: Long): IO[List[LineItem]] =
    solarius.run(Queries.getInvoiceLineItems(invoiceId))

  def getStoreBalance(storeId: Long): IO[BigDecimal] =
    solarius.run(Queries.getStoreBalance(storeId))

  def getInvoiceTaxes(invoiceId: Long): IO[ApplicableTaxes] =
    solarius.run(Queries.getInvoiceTaxes(invoiceId))

  def search(sr: SearchRequest, inclusions: Seq[String] = List()): IO[SearchResult[Invoice]] =
    solarius.run(Queries.search(sr, inclusions))

  private object Queries {
    def search(sr: SearchRequest, inclusions: Seq[String]): ConnectionIO[SearchResult[Invoice]] = {
      val nonEmptyFilters = sr.filters.mapValues(_.trim).filterNot(_._2.isEmpty)
      val wheres: List[Option[Fragment]] = List(
        Some(fr"1 = 1"),
        nonEmptyFilters.get("name").map(value => fr"i.sale_id LIKE ${"%" + value + "%"}"),
        nonEmptyFilters.get("storeId").map(value => fr"i.branch_id = $value"),
        nonEmptyFilters.get("status").flatMap(InvoiceStatus.fromString).map(value => fr"i.status = $value"),
        nonEmptyFilters.get("orderName").map(value => fr"o.sale_id LIKE ${"%" + value + "%"}"),
        nonEmptyFilters.get("customerName").map(value => fr"c.full_name LIKE ${"%" + value + "%"}"),
      )

      val allowedSortFields = Map(
        "id" -> "i.id",
        "orderId" -> "i.order_id",
        "orderName" -> "o.sale_id",
        "name" -> "i.sale_id",
        "type" -> "i.type_id",
        "currency" -> "i.currency_id",
        "subtotal" -> "i.sub_total",
        "total" -> "i.total",
        "balance" -> "i.balance",
        "paidAmount" -> "i.deposit",
        "createdAt" -> "i.creation_date",
        "updatedAt" -> "i.modification_date",
        "status" -> "i.status",
        "customerName" -> "c.full_name",
      )

      val (sortField, sortOrder) = sr.sortField
        .flatMap(allowedSortFields.get)
        .map(field => (field, sr.sortOrder))
        .getOrElse(("i.creation_date", SearchRequest.SORT_DESC))

      val limitClause = sr.limit
        .map(lim => fr"LIMIT ${sr.offset}, $lim")
        .getOrElse(fr"LIMIT 100")

      val sql =
        fr"""
          SELECT
            SQL_CALC_FOUND_ROWS
           i.id, i.url_id, i.sale_id, i.branch_id, i.user_id, sub_total, total, deposit, currency_id,
           i.type_id, i.status, i.creation_date, i.modification_date, i.note, i.customer_id, c.full_name, i.order_id, o.sale_id
          FROM s_invoices i
          JOIN customers c ON c.id = i.customer_id
          LEFT JOIN s_orders o ON o.id = i.order_id """ ++
          Fragments.whereAndOpt(wheres: _*) ++
          Fragment.const(s"ORDER BY $sortField $sortOrder") ++
          limitClause

      val fetchInvoices = sql
        .query[Invoice.InvoiceDB]
        .map(_.toEntity)
        .to[List]

      for {
        invoices <- fetchInvoices
        count <- sql"SELECT FOUND_ROWS()".query[Int].unique
      } yield SearchResult(invoices, count)
    }

    def getInvoice(field: String, invoiceId: String): ConnectionIO[Option[Invoice]] = {
      val whereClause: Fragment = field match {
        case "uuid" => fr"i.url_id = $invoiceId"
        case _ => fr"i.id = $invoiceId"
      }

      val sql =
        sql"""
         SELECT
           i.id, i.url_id, i.sale_id, i.branch_id, i.user_id, sub_total, total, deposit, currency_id,
           i.type_id, i.status, i.creation_date, i.modification_date, i.note, i.customer_id, c.full_name, i.order_id, o.sale_id
         FROM s_invoices i
         JOIN customers c ON c.id = i.customer_id
         LEFT JOIN s_orders o ON o.id = i.order_id
         WHERE """ ++ whereClause

      sql
        .query[InvoiceDB]
        .map(_.toEntity)
        .option
    }

    def getStoreBalance(storeId: Long): ConnectionIO[BigDecimal] = {
      val unpaidStatusId = InvoiceStatus.fromString(InvoiceStatus.NORMAL).get
      val sql = sql"SELECT IFNULL(SUM(balance), 0.00) AS totalBalance FROM s_invoices WHERE branch_id = $storeId AND status = $unpaidStatusId"

      sql.query[BigDecimal].unique
    }

    def getInvoiceTaxes(invoiceId: Long): ConnectionIO[ApplicableTaxes] = {
      val sql =
        sql"""
          SELECT
            c.id AS componentId,
            c.label AS componentName,
            c.value AS componentRate,
            it.total AS componentAmount
          FROM s_invoice_taxes AS it
          JOIN tax_components AS c ON c.id = it.tax_component_id
          WHERE it.invoice_id = $invoiceId;
        """

      sql
        .query[(TaxComponent, BigDecimal)]
        .to[List]
        .map(ApplicableTaxes(_))
    }

    def getInvoiceLineItems(invoiceId: Long): ConnectionIO[List[LineItem]] = {
      val sql = sql"SELECT id, quantity, retail_price, sale_price, status, product_id, product_label FROM s_invoice_products WHERE invoice_id = $invoiceId"

      val getItems: ConnectionIO[List[LineItem]] = sql
        .query[LineItem.LineItemDB]
        .map(_.toEntity)
        .to[List]

      for {
        items <- getItems // Get the line items
        attributeOverrides <- getLineItemsAttributeOverrides(invoiceId) // Get all the attribute overrides
      } yield {
        // Merge line items with their attribute overrides
        items.map { item =>
          item.copy(attributeOverrides = attributeOverrides.getOrElse(item.id, List()))
        }
      }
    }

    private def getLineItemsAttributeOverrides(invoiceId: Long): ConnectionIO[Map[Long, Seq[(String, String)]]] = {
      val sql =
        sql"""
          SELECT *
          FROM s_invoice_product_attributes AS ipa
          JOIN s_invoice_products ip ON ip.id = ipa.product_record_id
          WHERE ip.invoice_id = $invoiceId
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
