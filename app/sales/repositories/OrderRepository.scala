package sales.repositories

import java.sql.ResultSet
import java.time.LocalDateTime
import java.util.UUID
import javax.inject.Inject

import accounting.entities.InvoiceStatus
import infra.DatabaseExecutionContext
import inventory.util.{DB, SearchRequest, SearchResult}
import play.api.db.{Database, NamedDatabase}
import sales.entities._
import shared.entities.{LineItem, LineItemType}
import utils.OrderId

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class OrderRepository @Inject()(@NamedDatabase("solarius") db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(orderId: OrderId): Future[Option[Order]] = Future {
    db.withConnection { conn =>
      val sql =
        """
        SELECT
          o.id,
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
          WHERE o.id = @orderId
      """
      val params = Map(
        "orderId" -> orderId.toString
      )

      DB.fetchOne[Order](sql, params)(hydrateOrder)(conn)
    }
  }

  def get(orderId: UUID): Future[Option[Order]] = Future {
    db.withConnection { conn =>
      val sql =
        """
        SELECT
          o.id,
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
          WHERE o.url_id = @orderId
        """
      val params = Map(
        "orderId" -> orderId.toString
      )

      DB.fetchOne[Order](sql, params)(hydrateOrder)(conn)
    }
  }

  def getExpeditionDetails(orderId: OrderId): Future[Option[ExpeditionDetails]] = Future {
    db.withConnection { conn =>
      val sql =
        """
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
        WHERE o.id = @orderId;
        """

      val expeditionDetails = DB.fetchOne[ExpeditionDetails](sql, Map("orderId" -> orderId.toString))(hydrateExpeditionDetails)(conn)

      expeditionDetails.map { details =>
        val attributesSql = "SELECT attribute_code, attribute_value FROM s_order_expedition_attributes AS oea WHERE order_id = @orderId"

        val attributes = DB.fetchMany[(String, String)](attributesSql, Map("orderId" -> orderId.toString)) { rs =>
          (rs.getString("attribute_code"), rs.getString("attribute_value"))
        }(conn).toMap


        details.copy(metadata = attributes)
      }
    }
  }

  def search(sr: SearchRequest): Future[SearchResult[Order]] = Future {
    db.withConnection { conn =>
      val nonEmptyFilters = sr.filters.mapValues(_.trim).filterNot(_._2.isEmpty)
      val wheres = new ListBuffer[String]()
      val havings = new ListBuffer[String]()
      val joins = new ListBuffer[String]()
      var params: Map[String, String] = Map()

      wheres += "1 = 1"
      havings += "1 = 1"

      // `name` filter
      nonEmptyFilters.get("name").foreach(value => {
        wheres += "o.sale_id LIKE @name"
        params = params + ("name" -> s"%$value%")
      })

      // `storeId` filter
      nonEmptyFilters.get("storeId").foreach(value => {
        wheres += "o.branch_id = @storeId"
        params = params + ("storeId" -> value)
      })

      // `customerId` filter
      nonEmptyFilters.get("customerId").foreach(value => {
        wheres += "c.id = @customerId"
        params = params + ("customerId" -> value)
      })

      // `customerName` filter
      nonEmptyFilters.get("customerName").foreach(value => {
        wheres += "c.full_name LIKE @customerName"
        params = params + ("customerName" -> s"%$value%")
      })

      // `department` filter
      nonEmptyFilters.get("department").flatMap(OrderDepartment.fromString).foreach(departmentId => {
        wheres += "o.progress_id = @departmentId"
        params = params + ("departmentId" -> departmentId.toString)
      })

      // `type` filter
      nonEmptyFilters.get("type").flatMap(OrderType.fromString).foreach(typeId => {
        wheres += "o.type_id = @typeId"
        params = params + ("typeId" -> typeId.toString)
      })

      // `status` filter
      nonEmptyFilters.get("status").flatMap(OrderStatus.fromString).foreach(statusId => {
        wheres += "o.status = @statusId"
        params = params + ("statusId" -> statusId.toString)
      })

      // `progress` (rms-specific) filter
      nonEmptyFilters.get("progress").map {
        case "cancelled" =>
          wheres += "o.status = @statusId"
          params = params + ("statusId" -> OrderStatus.fromString(OrderStatus.CANCELLED).get.toString)
        case "new" =>
          wheres += "o.status = @statusId"
          params = params + ("statusId" -> OrderStatus.fromString(OrderStatus.NEW).get.toString)
        case "manufactured" =>
          wheres += "o.status = @statusId"
          params = params + ("statusId" -> OrderStatus.fromString(OrderStatus.APPROVED).get.toString)

          wheres += "o.progress_id = @departmentId"
          params = params + ("departmentId" -> OrderDepartment.fromString(OrderDepartment.EXPEDITION).get.toString)

          wheres += "(i.id IS NULL OR i.status = @invoiceStatus)"
          params = params + ("invoiceStatus" -> InvoiceStatus.fromString(InvoiceStatus.NORMAL).get.toString)
        case "readyToShip" =>
          wheres += "o.status = @statusId"
          params = params + ("statusId" -> OrderStatus.fromString(OrderStatus.APPROVED).get.toString)

          wheres += "o.progress_id = @departmentId"
          params = params + ("departmentId" -> OrderDepartment.fromString(OrderDepartment.EXPEDITION).get.toString)

          wheres += "i.status = @invoiceStatus"
          params = params + ("invoiceStatus" -> InvoiceStatus.fromString(InvoiceStatus.PAID).get.toString)
        case "shipped" =>
          wheres += "o.status = @statusId"
          params = params + ("statusId" -> OrderStatus.fromString(OrderStatus.APPROVED).get.toString)

          wheres += "o.progress_id = @departmentId"
          params = params + ("departmentId" -> OrderDepartment.fromString(OrderDepartment.COMPLETE).get.toString)
        case "inProgress" =>
          wheres += "o.status = @statusId"
          params = params + ("statusId" -> OrderStatus.fromString(OrderStatus.APPROVED).get.toString)

          wheres += "@minDepartmentId <= o.progress_id AND o.progress_id <= @maxDepartmentId"
          params = params ++ Seq(
            "minDepartmentId" -> OrderDepartment.fromString(OrderDepartment.DISTRIBUTION).get.toString,
            "maxDepartmentId" -> OrderDepartment.fromString(OrderDepartment.PRODUCTION).get.toString
          )
      }

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

      val sortField = sr.sortField.flatMap(allowedSortFields.get).getOrElse("o.creation_date")
      val countSql =
        s"""
              SELECT COUNT(*)
              FROM s_orders o
                JOIN customers c ON c.id = o.customer_id
                LEFT JOIN s_order_attributes oa ON oa.order_id = o.id AND oa.name = "model_id"
                LEFT JOIN s_invoices i ON i.id = o.invoice_id
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
        """

      val totalCount = DB.fetchColumn[Int](countSql, params)(conn)
      val fetchSql =
        s"""
              SELECT
                o.id,
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
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
              ORDER BY $sortField ${sr.sortOrder}
              ${sr.limit.map(lim => s"LIMIT ${sr.offset}, $lim").getOrElse("LIMIT 100")}
        """

      val orders = DB.fetchMany(fetchSql, params)(hydrateOrder)(conn)

      SearchResult(orders, totalCount.get)
    }
  }

  def getLineItems(orderId: OrderId): Future[Seq[LineItem]] = Future {
    db.withConnection { conn =>
      val sql = """SELECT * FROM s_order_products WHERE order_id = @orderId"""

      val lineItems = DB.fetchMany(sql, Map("orderId" -> orderId.toString))(hydrateLineItem)(conn)
      val lineItemAttributeOverrides = getLineItemsAttributeOverrides(orderId)

      lineItems.map(li => li.copy(attributeOverrides = lineItemAttributeOverrides.getOrElse(li.id, Seq())))
    }
  }

  private def getLineItemsAttributeOverrides(orderId: OrderId): Map[Long, Seq[(String, String)]] = db.withConnection { conn =>
    val sql =
      """
          SELECT *
          FROM s_order_product_attributes AS opa
          JOIN s_order_products op ON op.id = opa.product_record_id
          WHERE op.order_id = @orderId
        """

    val attributesData: Seq[(Long, String, String)] = DB.fetchMany(sql, Map("orderId" -> orderId.toString)) { rs =>
      (rs.getLong("product_record_id"), rs.getString("attribute_code"), rs.getString("attribute_value"))
    }(conn)

    attributesData.groupBy(_._1).mapValues(_.map(tuple => (tuple._2, tuple._3)))
  }

  private def hydrateOrder(rs: ResultSet): Order = {
    val status = OrderStatus.fromId(rs.getInt("o.status")).getOrElse(OrderStatus.NEW)
    val orderType = OrderType.fromId(rs.getInt("o.type_id")).getOrElse(OrderType.PARTS)
    val department = OrderDepartment.fromId(rs.getInt("o.progress_id")).getOrElse(OrderDepartment.CREATION)
    val invoiceId = DB.getNullable[Long]("invoiceId", rs).map("invoiceId" -> _.toString)
    val modelSku = DB.getNullable[String]("modelSku", rs).map("modelSku" -> _)
    val invoiceStatus = for {
      statusId <- DB.getNullable[Long]("invoiceStatus", rs)
      statusString <- InvoiceStatus.fromId(statusId.toInt)
    } yield "invoiceStatus" -> statusString
    val metadata = Map(
      "note" -> rs.getString("o.notes"),
      "customerName" -> rs.getString("c.full_name"),
      "authorName" -> rs.getString("authorName"),
      "cancelReason" -> DB.getNullable[String]("cancel_note", rs).getOrElse(""),
      "expeditedAt" -> DB.getNullable[String]("expedition_expedited_date", rs).getOrElse("")
    ) ++ invoiceId ++ invoiceStatus ++ modelSku

    Order(
      id = rs.getLong("id"),
      uuid = UUID.fromString(rs.getString("url_id")),
      name = rs.getString("sale_id"),
      authorId = rs.getLong("user_id"),
      storeId = rs.getLong("branch_id"),
      customerId = rs.getLong("customer_id"),
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = DB.getNullable[LocalDateTime]("modification_date", rs),
      tag = rs.getString("tag"),
      metadata = metadata,
      department = department,
      orderType = orderType,
      status = status
    )
  }

  private def hydrateLineItem(rs: ResultSet): LineItem = {
    val productId = rs.getLong("product_id")
    val retailPrice = DB.getNullable[BigDecimal]("retail_price", rs).getOrElse(BigDecimal(0.00))
    val salePrice = DB.getNullable[BigDecimal]("sale_price", rs).getOrElse(BigDecimal(0.00))

    LineItem(
      rs.getLong("id"),
      if (productId == 0) None else Some(productId),
      rs.getInt("quantity"),
      retailPrice,
      salePrice,
      LineItemType.fromId(rs.getInt("status")).getOrElse(LineItemType.NORMAL),
      metadata = Map(
        "productName" -> rs.getString("product_label")
      )
    )
  }

  private def hydrateExpeditionDetails(rs: ResultSet): ExpeditionDetails = {
    ExpeditionDetails(
      rs.getLong("em.id"),
      rs.getString("em.code")
    )
  }
}
