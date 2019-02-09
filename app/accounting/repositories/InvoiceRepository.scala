package accounting.repositories

import java.sql.ResultSet
import java.time.LocalDateTime
import java.util.UUID
import javax.inject.Inject

import accounting.entities._
import infrastructure.DatabaseExecutionContext
import inventory.util.{DB, SearchRequest, SearchResult}
import play.api.db.{Database, NamedDatabase}
import shared._
import shared.entities.{ApplicableTaxes, LineItem, LineItemType, TaxComponent}
import utils.InvoiceId

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

      DB.fetchOne[Invoice](sql, params)(hydrateInvoice)(conn)
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

      DB.fetchOne[Invoice](sql, params)(hydrateInvoice)(conn)
    }
  }

  def get(invoiceId: UUID, storeId: Long): Future[Option[Invoice]] = Future {
    db.withConnection { conn =>
      val sql =
        """
         SELECT i.*, c.*, o.sale_id AS orderName
         FROM s_invoices i
         JOIN customers c ON c.id = i.customer_id
         LEFT JOIN s_orders o ON o.id = i.order_id
         WHERE i.url_id = @invoiceId AND i.branch_id = @storeId
      """
      val params = Map(
        "storeId" -> storeId.toString,
        "invoiceId" -> invoiceId.toString
      )

      DB.fetchOne[Invoice](sql, params)(hydrateInvoice)(conn)
    }
  }

  def getStoreBalance(storeId: Long): Future[Option[BigDecimal]] = Future {
    db.withConnection { conn =>
      val unpaidStatusId = InvoiceStatus.fromString(InvoiceStatus.NORMAL).get
      val sql = "SELECT SUM(balance) AS totalBalance FROM s_invoices WHERE branch_id = @storeId AND status = @invoiceStatus"
      val params = Map("storeId" -> storeId.toString, "invoiceStatus" -> unpaidStatusId.toString)

      DB.fetchOne[Option[BigDecimal]](sql, params) { rs =>
        DB.getNullable[BigDecimal]("totalBalance", rs)
      }(conn).flatten
    }
  }

  def getTaxes(invoiceId: InvoiceId): Future[ApplicableTaxes] = Future {
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

      val taxes = DB.fetchMany(sql, Map("invoiceId" -> invoiceId.toString)) { rs =>
        (hydrateTaxComponent(rs), BigDecimal(rs.getBigDecimal("componentAmount")))
      }(conn)

      ApplicableTaxes(taxes)
    }
  }

  def getLineItems(invoiceId: InvoiceId): Future[Seq[LineItem]] = Future {
    db.withConnection { conn =>
      val sql = "SELECT * FROM s_invoice_products WHERE invoice_id = @invoiceId"

      val lineItems = DB.fetchMany(sql, Map("invoiceId" -> invoiceId.toString))(hydrateLineItem)(conn)
      val lineItemAttributeOverrides = getLineItemsAttributeOverrides(invoiceId)

      lineItems.map(li => li.copy(attributeOverrides = lineItemAttributeOverrides.getOrElse(li.id, Seq())))
    }
  }

  private def getLineItemsAttributeOverrides(invoiceId: InvoiceId): Map[Long, Seq[(String, String)]] = db.withConnection { conn =>
    val sql =
      """
          SELECT *
          FROM s_invoice_product_attributes AS ipa
          JOIN s_invoice_products ip ON ip.id = ipa.product_record_id
          WHERE ip.invoice_id = @invoiceId
        """

    val attributesData: Seq[(Long, String, String)] = DB.fetchMany(sql, Map("invoiceId" -> invoiceId.toString)) { rs =>
      (rs.getLong("product_record_id"), rs.getString("attribute_code"), rs.getString("attribute_value"))
    }(conn)

    attributesData.groupBy(_._1).mapValues(_.map(tuple => (tuple._2, tuple._3)))
  }

  def search(sr: SearchRequest, inclusions: Seq[String]): Future[SearchResult[Invoice]] = Future {
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
        wheres += "i.sale_id LIKE @name"
        params = params + ("name" -> s"%$value%")
      })

      // `storeId` filter
      nonEmptyFilters.get("storeId").foreach(value => {
        wheres += "i.branch_id = @storeId"
        params = params + ("storeId" -> value)
      })

      // `customerName` filter
      nonEmptyFilters.get("customerName").foreach(value => {
        wheres += "c.full_name LIKE @customerName"
        params = params + ("customerName" -> s"%$value%")
      })

      // `orderName` filter
      nonEmptyFilters.get("orderName").foreach(value => {
        wheres += "o.sale_id LIKE @orderName"
        params = params + ("orderName" -> s"%$value%")
      })

      // `status` filter
      nonEmptyFilters.get("status").flatMap(InvoiceStatus.fromString).foreach(statusId => {
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

      val sortField = sr.sortField.flatMap(allowedSortFields.get).getOrElse("i.creation_date")
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

      val totalCount = DB.fetchColumn[Int](countSql, params)(conn)
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
      val invoices = DB.fetchMany(fetchSql, params)(hydrateInvoice)(conn)

      SearchResult(invoices, totalCount.get)
    }
  }

  private def hydrateInvoice(rs: ResultSet): Invoice = {
    val currency = if (rs.getInt("currency_id") == 1) Currency.CAD else Currency.USD
    val invoiceType = if (rs.getInt("type_id") == 1) InvoiceType.INVOICE else InvoiceType.CONTRACT
    val invoiceStatus = InvoiceStatus.fromId(rs.getInt("status")).getOrElse(InvoiceStatus.NORMAL)
    val metadata = Map(
      "note" -> rs.getString("i.note"),
      "customerName" -> rs.getString("c.full_name"),
      "orderName" -> DB.getNullable[String]("orderName", rs).getOrElse("")
    )

    Invoice(
      id = rs.getLong("id"),
      uuid = UUID.fromString(rs.getString("url_id")),
      orderId = DB.getNullable[Long]("order_id", rs),
      authorId = rs.getLong("user_id"),
      storeId = rs.getLong("branch_id"),
      customerId = rs.getLong("customer_id"),
      name = rs.getString("sale_id"),
      subtotal = BigDecimal(rs.getBigDecimal("sub_total")),
      total = BigDecimal(rs.getBigDecimal("total")),
      paidAmount = DB.getNullable[BigDecimal]("deposit", rs).getOrElse(BigDecimal(0)),
      currency = currency,
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = DB.getNullable[LocalDateTime]("modification_date", rs),
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

  private def hydrateLineItem(rs: ResultSet): LineItem = {
    val productId = rs.getLong("product_id")

    LineItem(
      rs.getLong("id"),
      if (productId == 0) None else Some(productId),
      rs.getInt("quantity"),
      BigDecimal(rs.getBigDecimal("retail_price")),
      BigDecimal(rs.getBigDecimal("sale_price")),
      LineItemType.fromId(rs.getInt("status")).getOrElse(LineItemType.NORMAL),
      metadata = Map(
        "productName" -> rs.getString("product_label")
      )
    )
  }
}
