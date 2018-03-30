package sales.repositories

import java.sql.ResultSet
import java.time.LocalDateTime
import java.util.UUID
import javax.inject.Inject
import accounting.entities.InvoiceStatus
import infrastructure.DatabaseExecutionContext
import inventory.util.{DatabaseHelper, SearchRequest, SearchResult}
import play.api.db.{Database, NamedDatabase}
import sales.entities.{Order, OrderDepartment, OrderStatus, OrderType}
import shared.OrderId
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class OrderRepository @Inject()(@NamedDatabase("solarius") db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(id: OrderId): Future[Option[Order]] = Future {
    db.withConnection { conn =>
      val sql =
        """
         SELECT o.*, c.*
         FROM s_orders o
         JOIN customers c ON c.id = o.customer_id
         WHERE o.id = @orderId
      """
      val params = Map(
        "orderId" -> id.toString
      )

      DatabaseHelper.fetchOne[Order](sql, params)(hydrateOrder)(conn)
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

      val totalCount = DatabaseHelper.fetchColumn[Int](countSql, params)(conn)
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

      val orders = DatabaseHelper.fetchMany(fetchSql, params)(hydrateOrder)(conn)

      SearchResult(orders, totalCount.get)
    }
  }

  private def hydrateOrder(rs: ResultSet): Order = {
    val status = OrderStatus.fromId(rs.getInt("o.status")).getOrElse(OrderStatus.NEW)
    val orderType = OrderType.fromId(rs.getInt("o.type_id")).getOrElse(OrderType.PARTS)
    val department = OrderDepartment.fromId(rs.getInt("o.progress_id")).getOrElse(OrderDepartment.CREATION)
    val invoiceId = DatabaseHelper.getNullable[Long]("invoiceId", rs).map("invoiceId" -> _.toString)
    val modelSku = DatabaseHelper.getNullable[String]("modelSku", rs).map("modelSku" -> _)
    val invoiceStatus = for {
      statusId <- DatabaseHelper.getNullable[Long]("invoiceStatus", rs)
      statusString <- InvoiceStatus.fromId(statusId.toInt)
    } yield "invoiceStatus" -> statusString
    val metadata = Map(
      "note" -> rs.getString("o.notes"),
      "customerName" -> rs.getString("c.full_name"),
      "authorName" -> rs.getString("authorName"),
    ) ++ invoiceId ++ invoiceStatus ++ modelSku

    Order(
      id = rs.getLong("id"),
      uuid = UUID.fromString(rs.getString("url_id")),
      name = rs.getString("sale_id"),
      authorId = rs.getLong("user_id"),
      storeId = rs.getLong("branch_id"),
      customerId = rs.getLong("customer_id"),
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = DatabaseHelper.getNullable[LocalDateTime]("modification_date", rs),
      tag = rs.getString("tag"),
      metadata = metadata,
      department = department,
      orderType = orderType,
      status = status
    )
  }
}
