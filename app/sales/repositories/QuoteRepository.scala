package sales.repositories

import java.sql.ResultSet
import java.time.LocalDateTime
import java.util.UUID
import javax.inject.Inject

import accounting.entities._
import infrastructure.DatabaseExecutionContext
import inventory.util.{DB, SearchRequest, SearchResult}
import play.api.db.{Database, NamedDatabase}
import sales.entities.{Quote, QuoteStatus}
import shared._
import shared.entities._
import utils.QuoteId

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class QuoteRepository @Inject()(@NamedDatabase("solarius") db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(id: QuoteId): Future[Option[Quote]] = Future {
    db.withConnection { conn =>
      val sql =
        """
         SELECT q.*, c.*
         FROM s_quotes q
         JOIN customers c ON c.id = q.customer_id
         WHERE q.id = @quoteId
      """
      val params = Map(
        "quoteId" -> id.toString
      )

      DB.fetchOne[Quote](sql, params)(hydrateQuote)(conn)
    }
  }

  def get(quoteId: UUID): Future[Option[Quote]] = Future {
    db.withConnection { conn =>
      val sql =
        """
         SELECT q.*, c.*
         FROM s_quotes q
         JOIN customers c ON c.id = q.customer_id
         WHERE q.url_id = @quoteId
      """
      val params = Map(
        "quoteId" -> quoteId.toString
      )

      DB.fetchOne[Quote](sql, params)(hydrateQuote)(conn)
    }
  }

  def get(quoteId: QuoteId, storeId: Long): Future[Option[Quote]] = Future {
    db.withConnection { conn =>
      val sql =
        """
         SELECT q.*, c.*
         FROM s_quotes q
         JOIN customers c ON c.id = q.customer_id
         WHERE q.id = @quoteId AND q.branch_id = @storeId
      """
      val params = Map(
        "storeId" -> storeId.toString,
        "quoteId" -> quoteId.toString
      )

      DB.fetchOne[Quote](sql, params)(hydrateQuote)(conn)
    }
  }

  def get(quoteId: UUID, storeId: Long): Future[Option[Quote]] = Future {
    db.withConnection { conn =>
      val sql =
        """
         SELECT q.*, c.*
         FROM s_quotes q
         JOIN customers c ON c.id = q.customer_id
         WHERE q.url_id = @quoteId AND q.branch_id = @storeId
      """
      val params = Map(
        "storeId" -> storeId.toString,
        "quoteId" -> quoteId.toString
      )

      DB.fetchOne[Quote](sql, params)(hydrateQuote)(conn)
    }
  }

  def getAttachments(quoteId: QuoteId): Future[Seq[File]] = Future {
    db.withConnection { conn =>
      val sql = "SELECT * FROM s_quote_documents WHERE quote_id = @quoteId"

      DB.fetchMany(sql, Map("quoteId" -> quoteId.toString))(hydrateFile)(conn)
    }
  }

  def getTaxes(quoteId: QuoteId): Future[ApplicableTaxes] = Future {
    db.withConnection { conn =>
      val sql =
        """
          SELECT
          	c.id AS componentId,
          	c.label AS componentName,
          	c.value AS componentRate,
          	qt.total AS componentAmount,
          	t.id AS taxId,
          	t.label AS taxName
          FROM s_quote_taxes AS qt
          JOIN tax_components AS c ON c.id = qt.tax_component_id
          JOIN tax AS t ON t.id = c.tax_id
          WHERE qt.quote_id = @quoteId;
        """

      val taxes = DB.fetchMany(sql, Map("quoteId" -> quoteId.toString)) { rs =>
        (hydrateTaxComponent(rs), BigDecimal(rs.getBigDecimal("componentAmount")))
      }(conn)

      ApplicableTaxes(taxes)
    }
  }

  def getLineItems(quoteId: QuoteId): Future[Seq[LineItem]] = Future {
    db.withConnection { conn =>
      val sql = "SELECT * FROM s_quote_products WHERE quote_id = @quoteId"

      val lineItems = DB.fetchMany(sql, Map("quoteId" -> quoteId.toString))(hydrateLineItem)(conn)
      val lineItemAttributeOverrides = getLineItemsAttributeOverrides(quoteId)

      lineItems.map(li => li.copy(attributeOverrides = lineItemAttributeOverrides.getOrElse(li.id, Seq())))
    }
  }

  private def getLineItemsAttributeOverrides(quoteId: QuoteId): Map[Long, Seq[(String, String)]] = db.withConnection { conn =>
    val sql =
      """
          SELECT *
          FROM s_quote_product_attributes AS qpa
          JOIN s_quote_products qp ON qp.id = qpa.product_record_id
          WHERE qp.quote_id = @quoteId
        """

    val attributesData: Seq[(Long, String, String)] = DB.fetchMany(sql, Map("quoteId" -> quoteId.toString)) { rs =>
      (rs.getLong("product_record_id"), rs.getString("attribute_code"), rs.getString("attribute_value"))
    }(conn)

    attributesData.groupBy(_._1).mapValues(_.map(tuple => (tuple._2, tuple._3)))
  }

  def search(sr: SearchRequest, inclusions: Seq[String]): Future[SearchResult[Quote]] = Future {
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
        wheres += "q.sale_id LIKE @name"
        params = params + ("name" -> s"%$value%")
      })

      // `storeId` filter
      nonEmptyFilters.get("storeId").foreach(value => {
        wheres += "q.branch_id = @storeId"
        params = params + ("storeId" -> value)
      })

      // `customerName` filter
      nonEmptyFilters.get("customerName").foreach(value => {
        wheres += "c.full_name LIKE @customerName"
        params = params + ("customerName" -> s"%$value%")
      })

      // `status` filter
      nonEmptyFilters.get("status").flatMap(QuoteStatus.fromString).foreach(statusId => {
        wheres += "q.status = @statusId"
        params = params + ("statusId" -> statusId.toString)
      })

      val allowedSortFields = Map(
        "id" -> "q.id",
        "name" -> "q.sale_id",
        "type" -> "q.type_id",
        "currency" -> "q.currency_id",
        "subtotal" -> "q.sub_total",
        "total" -> "q.total",
        "createdAt" -> "q.creation_date",
        "updatedAt" -> "q.modification_date",
        "status" -> "q.status",
        "customerName" -> "c.full_name"
      )

      val sortField = sr.sortField.flatMap(allowedSortFields.get).getOrElse("q.creation_date")
      val countSql =
        s"""
              SELECT COUNT(*)
              FROM s_quotes q
              JOIN customers c ON c.id = q.customer_id
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
        """

      val totalCount = DB.fetchColumn[Int](countSql, params)(conn)
      val fetchSql =
        s"""
              SELECT
                q.*,
                c.*
              FROM s_quotes q
              JOIN customers c ON c.id = q.customer_id
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
              ORDER BY $sortField ${sr.sortOrder}
              ${sr.limit.map(lim => s"LIMIT ${sr.offset}, $lim").getOrElse("LIMIT 100")}
        """
      val quotes = DB.fetchMany(fetchSql, params)(hydrateQuote)(conn)

      SearchResult(quotes, totalCount.get)
    }
  }

  private def hydrateQuote(rs: ResultSet): Quote = {
    val currency = if (rs.getInt("currency_id") == 1) Currency.CAD else Currency.USD
    val quoteStatus = QuoteStatus.fromId(rs.getInt("status")).getOrElse(QuoteStatus.NORMAL)
    val metadata = Map(
      "note" -> rs.getString("q.note"),
      "customerName" -> rs.getString("c.full_name"),
    )

    Quote(
      id = rs.getLong("id"),
      uuid = UUID.fromString(rs.getString("url_id")),
      authorId = rs.getLong("user_id"),
      storeId = rs.getLong("branch_id"),
      customerId = rs.getLong("customer_id"),
      name = rs.getString("sale_id"),
      subtotal = BigDecimal(rs.getBigDecimal("sub_total")),
      total = BigDecimal(rs.getBigDecimal("total")),
      currency = currency,
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = DB.getNullable[LocalDateTime]("modification_date", rs),
      status = quoteStatus,
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
      LineItemType.fromId(rs.getInt("flag")).getOrElse(LineItemType.NORMAL),
      metadata = Map(
        "productName" -> rs.getString("product_label")
      )
    )
  }

  private def hydrateFile(rs: ResultSet): File = {
    File(
      rs.getString("file_name"),
      rs.getString("description"),
      rs.getString("url"),
      rs.getLong("size"),
      rs.getTimestamp("creation_date").toLocalDateTime
    )
  }
}