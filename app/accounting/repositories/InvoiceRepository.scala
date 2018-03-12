package accounting.repositories

import java.sql.ResultSet
import java.time.LocalDateTime
import javax.inject.Inject

import accounting.entities._
import infrastructure.DatabaseExecutionContext
import inventory.util.{DatabaseHelper, SearchRequest, SearchResult}
import play.api.Logger
import play.api.db.{Database, NamedDatabase}
import shared.{InvoiceId, Repository}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class InvoiceRepository @Inject()(@NamedDatabase("solarius") db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(id: InvoiceId): Future[Option[Invoice]] = Future {
    db.withConnection { conn =>
      val sql =
        """
         SELECT i.*, c.*, o.sale_id AS orderName
         FROM s_invoices i
         JOIN customers c ON c.id = i.customer_id
         LEFT JOIN s_orders o ON o.id = i.order_id
         WHERE i.id = @invoiceId
      """
      val params = Map(
        "invoiceId" -> id.toString
      )

      DatabaseHelper.fetchOne[Invoice](sql, params)(hydrateInvoice)(conn)
    }
  }

  def getInvoiceTaxes(invoiceId: Long): Future[Seq[InvoiceTax]] = Future {
    db.withConnection { conn =>
      val sql =
        """
          SELECT
          	c.id AS componentId,
          	c.label AS componentName,
          	c.value AS componentRate,
          	it.total AS componentAmount,
          	t.id AS taxId,
          	t.label AS taxName
          FROM s_invoice_taxes AS it
          JOIN tax_components AS c ON c.id = it.tax_component_id
          JOIN tax AS t ON t.id = c.tax_id
          WHERE it.invoice_id = @invoiceId;
        """

      DatabaseHelper.fetchMany(sql, Map("invoiceId" -> invoiceId.toString)) { rs =>
        InvoiceTax(hydrateTaxComponent(rs), BigDecimal(rs.getBigDecimal("componentAmount")))
      }(conn)
    }
  }

  def get(invoiceId: InvoiceId, storeId: Long): Future[Option[Invoice]] = Future {
    db.withConnection { conn =>
      val sql =
        """
         SELECT i.*, c.*, o.sale_id AS orderName
         FROM s_invoices i
         JOIN customers c ON c.id = i.customer_id
         LEFT JOIN s_orders o ON o.id = i.order_id
         WHERE i.id = @invoiceId AND i.branch_id = @storeId
      """
      val params = Map(
        "storeId" -> storeId.toString,
        "invoiceId" -> invoiceId.toString
      )

      DatabaseHelper.fetchOne[Invoice](sql, params)(hydrateInvoice)(conn)
    }
  }

  def getByStore(invoiceId: InvoiceId, storeId: Long): Option[Invoice] = db.withConnection { conn =>
    val sql =
      """
     SELECT
      i.*,
      c.*,
      o.sale_id AS orderName
     FROM s_invoices i
     JOIN customers c ON c.id = i.customer_id
     LEFT JOIN s_orders o ON o.id = i.order_id
     WHERE i.id = @invoiceId AND i.branch_id = @storeId
  """
    val params = Map(
      "storeId" -> storeId.toString,
      "invoiceId" -> invoiceId.toString
    )

    DatabaseHelper.fetchOne[Invoice](sql, params)(hydrateInvoice)(conn)
  }

  def search(sr: SearchRequest, inclusions: Seq[String]): Future[SearchResult[Invoice]] = Future {
    db.withConnection { conn =>
      val wheres = new ListBuffer[String]()
      val havings = new ListBuffer[String]()
      val joins = new ListBuffer[String]()
      var params: Map[String, String] = Map()

      wheres += "1 = 1"
      havings += "1 = 1"

      // `name` filter
      sr.filters.get("name").foreach(value => {
        wheres += "i.sale_id LIKE @name"
        params = params + ("name" -> s"%$value%")
      })

      // `storeId` filter
      sr.filters.get("storeId").foreach(value => {
        wheres += "i.branch_id = @storeId"
        params = params + ("storeId" -> value.toString)
      })

      // `customerName` filter
      sr.filters.get("customerName").foreach(value => {
        wheres += "c.full_name LIKE @customerName"
        params = params + ("customerName" -> s"%$value%")
      })

      // `orderName` filter
      sr.filters.get("orderName").foreach(value => {
        wheres += "o.sale_id LIKE @orderName"
        params = params + ("orderName" -> s"%$value%")
      })

      // `status` filter
      sr.filters.get("status").flatMap(InvoiceStatus.fromString(_)).foreach(statusId => {
        wheres += "i.status = @statusId"
        params = params + ("statusId" -> statusId.toString)
      })

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

      val sortField = sr.sortField.flatMap(allowedSortFields.get(_)).getOrElse("i.creation_date")
      val countSql =
        s"""
              SELECT COUNT(*)
              FROM s_invoices i
              JOIN customers c ON c.id = i.customer_id
              LEFT JOIN s_orders o ON o.id = i.order_id
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
        """

      val totalCount = DatabaseHelper.fetchColumn[Int](countSql, params)(conn)

      val fetchSql =
        s"""
              SELECT
                i.*,
                c.*,
                o.sale_id AS orderName
              FROM s_invoices i
              JOIN customers c ON c.id = i.customer_id
              LEFT JOIN s_orders o ON o.id = i.order_id
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
              ORDER BY $sortField ${sr.sortOrder}
              ${sr.limit.map(lim => s"LIMIT ${sr.offset}, $lim").getOrElse("LIMIT 100")}
        """

      val invoices = DatabaseHelper.fetchMany(fetchSql, params)(hydrateInvoice)(conn)

      SearchResult(invoices, totalCount.get)
    }
  }

  private def hydrateInvoice(rs: ResultSet): Invoice = {
    val currency = if (rs.getInt("currency_id") == 1) Currency.CAD else Currency.USD
    val invoiceType = if (rs.getInt("type_id") == 1) InvoiceType.INVOICE else InvoiceType.CONTRACT
    val invoiceStatus = InvoiceStatus.fromId(rs.getInt("status")).getOrElse(InvoiceStatus.NORMAL)
    val metadata = Map(
      "customerName" -> rs.getString("c.full_name"),
      "orderName" -> DatabaseHelper.getNullable[String]("orderName", rs).getOrElse("")
    )

    Invoice(
      id = rs.getLong("id"),
      orderId = DatabaseHelper.getNullable[Long]("order_id", rs),
      authorId = rs.getLong("user_id"),
      storeId = rs.getLong("branch_id"),
      customerId = rs.getLong("customer_id"),
      name = rs.getString("sale_id"),
      subtotal = BigDecimal(rs.getBigDecimal("sub_total")),
      total = BigDecimal(rs.getBigDecimal("total")),
      paidAmount = DatabaseHelper.getNullable[BigDecimal]("deposit", rs).getOrElse(BigDecimal(0)),
      currency = currency,
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = DatabaseHelper.getNullable[LocalDateTime]("modification_date", rs),
      invoiceType = invoiceType,
      status = invoiceStatus,
      metadata = metadata
    )
  }

  private def hydrateTaxComponent(rs: ResultSet): TaxComponent =
    TaxComponent(
      rs.getLong("componentId"),
      rs.getString("componentName"),
      BigDecimal(rs.getBigDecimal("componentRate"))
    )

}
