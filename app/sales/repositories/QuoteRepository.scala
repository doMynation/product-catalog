package sales.repositories

import java.util.UUID
import javax.inject.Inject
import cats.effect.IO
import inventory.util.{SearchRequest, SearchResult}
import sales.entities.{Quote, QuoteStatus}
import shared.entities._
import utils.{SolariusDB}
import doobie._
import doobie.implicits._
import utils.imports.implicits._

final class QuoteRepository @Inject()(solarius: SolariusDB) {

  def getById(id: Long): IO[Option[Quote]] =
    solarius.run(Queries.getQuote("id", id.toString))

  def getById(id: UUID): IO[Option[Quote]] =
    solarius.run(Queries.getQuote("uuid", id.toString))

  def getAttachments(quoteId: Long): IO[List[File]] =
    solarius.run(Queries.getAttachments(quoteId))

  def getQuoteTaxes(quoteId: Long): IO[ApplicableTaxes] =
    solarius.run(Queries.getQuoteTaxes(quoteId))

  def search(sr: SearchRequest, inclusions: Seq[String] = List()): IO[SearchResult[Quote]] =
    solarius.run(Queries.search(sr, inclusions))

  def getQuoteLineItems(quoteId: Long): IO[List[LineItem]] =
    solarius.run(Queries.getQuoteLineItems(quoteId))

  private object Queries {
    def getQuote(idType: String, id: String): ConnectionIO[Option[Quote]] = {
      val where = idType match {
        case "uuid" => fr"url_id = $id"
        case _ => fr"id = $id"
      }

      val sql =
        fr"""
         SELECT
           q.id, q.url_id, q.sale_id, q.sub_total, q.total, q.currency_id, q.note, q.status,
           q.user_id, q.branch_id, c.id, c.full_name, q.creation_date, q.modification_date
         FROM s_quotes q
         JOIN customers c ON c.id = q.customer_id
         WHERE """ ++ where

      sql
        .query[Quote.QuoteDB]
        .map(_.toEntity)
        .option
    }

    def search(sr: SearchRequest, inclusions: Seq[String]): ConnectionIO[SearchResult[Quote]] = {
      val nonEmptyFilters = sr.filters.mapValues(_.trim).filterNot(_._2.isEmpty)
      val wheres: List[Option[Fragment]] = List(
        Some(fr"1 = 1"),
        nonEmptyFilters.get("name").map(value => fr"q.sale_id LIKE ${"%" + value + "%"}"),
        nonEmptyFilters.get("storeId").map(value => fr"q.branch_id = $value"),
        nonEmptyFilters.get("status").flatMap(QuoteStatus.fromString).map(value => fr"q.status = $value"),
        nonEmptyFilters.get("customerId").map(value => fr"c.id = $value"),
        nonEmptyFilters.get("customerName").map(value => fr"c.full_name LIKE ${"%" + value + "%"}"),
      )

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

      val (sortField, sortOrder) = sr.sortField
        .flatMap(allowedSortFields.get)
        .map(field => (field, sr.sortOrder))
        .getOrElse(("q.creation_date", SearchRequest.SORT_ASC))

      val limitClause = sr.limit
        .map(lim => fr"LIMIT ${sr.offset}, $lim")
        .getOrElse(fr"LIMIT 100")

      val sql =
        fr"""
          SELECT
            q.id, q.url_id, q.sale_id, q.sub_total, q.total, q.currency_id, q.note, q.status,
            q.user_id, q.branch_id, c.id, c.full_name, q.creation_date, q.modification_date
          FROM s_quotes q
          JOIN customers c ON c.id = q.customer_id""" ++
          Fragments.whereAndOpt(wheres: _*) ++
          Fragment.const(s"ORDER BY $sortField $sortOrder") ++
          limitClause

      val fetchQuotes: ConnectionIO[List[Quote]] = sql
        .query[Quote.QuoteDB]
        .map(_.toEntity)
        .to[List]

      for {
        quotes <- fetchQuotes // Fetch all orders
        count <- sql"""SELECT FOUND_ROWS()""".query[Int].unique // Get the total number of records
      } yield SearchResult(quotes, count)
    }

    def getAttachments(quoteId: Long): ConnectionIO[List[File]] = {
      val sql = sql"SELECT file_name, description, url, size, creation_date FROM s_quote_documents WHERE quote_id = $quoteId"

      sql.query[File].to[List]
    }

    def getQuoteTaxes(quoteId: Long): ConnectionIO[ApplicableTaxes] = {
      val sql =
        sql"""
          SELECT c.id, c.label, c.value, it.total
          FROM s_quote_taxes AS it
          JOIN tax_components AS c ON c.id = it.tax_component_id
          WHERE it.quote_id = $quoteId;
        """

      sql
        .query[(TaxComponent, BigDecimal)]
        .to[List]
        .map(ApplicableTaxes(_))
    }

    def getQuoteLineItems(quoteId: Long): ConnectionIO[List[LineItem]] = {
      val sql = sql"SELECT id, quantity, retail_price, sale_price, status, product_id, product_label FROM s_quote_products WHERE quote_id = $quoteId"

      val getItems: ConnectionIO[List[LineItem]] = sql
        .query[LineItem.LineItemDB]
        .map(_.toEntity)
        .to[List]

      for {
        items <- getItems // Get the line items
        attributeOverrides <- getLineItemsAttributeOverrides(quoteId) // Get all the attribute overrides
      } yield {
        // Merge line items with their attribute overrides
        items.map { item =>
          item.copy(attributeOverrides = attributeOverrides.getOrElse(item.id, List()))
        }
      }
    }

    private def getLineItemsAttributeOverrides(quoteId: Long): ConnectionIO[Map[Long, Seq[(String, String)]]] = {
      val sql =
        sql"""
          SELECT *
          FROM s_quote_product_attributes AS ipa
          JOIN s_quote_products ip ON ip.id = ipa.product_record_id
          WHERE ip.quote_id = $quoteId
        """

      sql
        .query[(Long, String, String)]
        .to[List]
        .map(_
          .groupBy(_._1)
          .mapValues(_.map(tuple => (tuple._2, tuple._3)))
        )
    }
  }

}
