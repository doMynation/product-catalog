package shared.repositories

import java.sql.ResultSet
import cats.effect.IO
import doobie._
import doobie.implicits._
import javax.inject.Inject
import infra.DatabaseExecutionContext
import inventory.util.{DB, SearchRequest, SearchResult}
import play.api.db.{Database, NamedDatabase}
import shared.entities.Comment
import utils.SolariusDB
import utils.imports.implicits._
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class CommentRepository @Inject()(@NamedDatabase("solarius") db: Database, solarius: SolariusDB)(implicit ec: DatabaseExecutionContext) {
  val TYPE_ORDER = "order"
  val TYPE_CUSTOMER = "none"
  val TYPE_QUOTE = "quote"
  val TYPE_QUOTATION_REQUEST = "quotationRequest"

  def searchIO(sr: SearchRequest): IO[SearchResult[Comment]] =
    solarius.run(Queries.search(sr))

  def search(sr: SearchRequest): Future[SearchResult[Comment]] = Future {
    db.withConnection { conn =>
      val nonEmptyFilters = sr.filters.mapValues(_.trim).filterNot(_._2.isEmpty)
      val wheres = new ListBuffer[String]()
      val havings = new ListBuffer[String]()
      val joins = new ListBuffer[String]()
      var params: Map[String, String] = Map()

      wheres += "1 = 1"
      havings += "1 = 1"

      // `type` filter
      nonEmptyFilters.get("type").foreach(value => {
        wheres += "com.document_type = @type"
        params = params + ("type" -> value)
      })

      // `documentId` filter
      nonEmptyFilters.get("documentId").foreach(value => {
        wheres += "com.document_id = @documentId"
        params = params + ("documentId" -> value)
      })

      // `authorId` filter
      nonEmptyFilters.get("authorId").foreach(value => {
        wheres += "com.user_id = @authorId"
        params = params + ("authorId" -> value)
      })

      // `customerId` filter
      nonEmptyFilters.get("customerId").foreach(value => {
        wheres += "com.customer_id = @customerId"
        params = params + ("customerId" -> value)
      })

      // `content` filter
      nonEmptyFilters.get("content").foreach(value => {
        wheres += "com.message LIKE @content"
        params = params + ("content" -> s"%$value%")
      })

      val allowedSortFields = Map(
        "id" -> "com.id",
        "authorId" -> "com.user_id",
        "customerId" -> "com.customer_id",
        "content" -> "com.message",
        "type" -> "com.document_type",
        "createdAt" -> "com.creation_date"
      )

      val sortField = sr.sortField.flatMap(allowedSortFields.get).getOrElse("com.creation_date")
      val sql =
        s"""
          SELECT
            SQL_CALC_FOUND_ROWS
            com.*,
            u.id AS authorId,
            u.full_name AS authorName,
            s.id AS authorStoreId,
            s.label AS authorStoreName,
            c.id AS customerId,
            c.full_name AS customerName
          FROM customer_notes AS com
          JOIN customers c ON c.id = com.customer_id
          JOIN users u ON u.id = com.user_id
          JOIN branches s ON s.id = u.branch_id
          ${joins.mkString(" ")}
          WHERE ${wheres.mkString(" AND ")}
          HAVING ${havings.mkString(" AND ")}
          ORDER BY $sortField ${sr.sortOrder}
          ${sr.limit.map(lim => s"LIMIT ${sr.offset}, $lim").getOrElse("LIMIT 100")}
        """
      val comments = DB.fetchMany(sql, params)(hydrateComment)(conn)
      val totalCount = DB.fetchColumn[Int]("SELECT FOUND_ROWS()")(conn)

      SearchResult(comments, totalCount.get)
    }
  }

  private def hydrateComment(rs: ResultSet): Comment =
    Comment(
      rs.getLong("id"),
      rs.getLong("authorId"),
      rs.getString("authorName"),
      rs.getLong("authorStoreId"),
      rs.getString("authorStoreName"),
      rs.getLong("customerId"),
      rs.getString("customerName"),
      rs.getString("message"),
      rs.getTimestamp("creation_date").toLocalDateTime
    )

  private object Queries {
    def search(sr: SearchRequest): ConnectionIO[SearchResult[Comment]] = {
      val nonEmptyFilters = sr.filters.mapValues(_.trim).filterNot(_._2.isEmpty)
      val wheres: List[Option[Fragment]] = List(
        Some(fr"1 = 1"),
        nonEmptyFilters.get("type").map(value => fr"com.document_type = $value"),
        nonEmptyFilters.get("documentId").map(value => fr"com.document_id = $value"),
        nonEmptyFilters.get("authorId").map(value => fr"com.user_id = $value"),
        nonEmptyFilters.get("customerId").map(value => fr"com.customer_id = $value"),
        nonEmptyFilters.get("content").map(value => fr"com.message LIKE ${"%" + value + "%"}"),
      )

      val allowedSortFields = Map(
        "id" -> "com.id",
        "authorId" -> "com.user_id",
        "customerId" -> "com.customer_id",
        "content" -> "com.message",
        "type" -> "com.document_type",
        "createdAt" -> "com.creation_date"
      )

      val (sortField, sortOrder) = sr.sortField
        .flatMap(allowedSortFields.get)
        .map(field => (field, sr.sortOrder))
        .getOrElse(("com.creation_date", SearchRequest.SORT_ASC))

      val limitClause = sr.limit
        .map(lim => fr"LIMIT ${sr.offset}, $lim")
        .getOrElse(fr"LIMIT 100")

      val sql =
        fr"""
          SELECT
            SQL_CALC_FOUND_ROWS
            com.id,
            u.id,
            u.full_name,
            s.id,
            s.label,
            c.id,
            c.full_name,
            com.message,
            com.creation_date
          FROM customer_notes AS com
          JOIN customers c ON c.id = com.customer_id
          JOIN users u ON u.id = com.user_id
          JOIN branches s ON s.id = u.branch_id""" ++
          Fragments.whereAndOpt(wheres: _*) ++
          Fragment.const(s"ORDER BY $sortField $sortOrder") ++
          limitClause

      val fetchComments: ConnectionIO[List[Comment]] = sql
        .query[Comment]
        .to[List]

      for {
        comments <- fetchComments // Fetch all products
        count <- sql"""SELECT FOUND_ROWS()""".query[Int].unique // Get the total number of records
      } yield SearchResult(comments, count)
    }
  }

}
