package shared.repositories

import java.sql.ResultSet
import javax.inject.Inject
import infrastructure.DatabaseExecutionContext
import inventory.util.{DatabaseHelper, SearchRequest, SearchResult}
import play.api.db.{Database, NamedDatabase}
import shared.Comment
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class CommentRepository @Inject()(@NamedDatabase("solarius") db: Database)(implicit ec: DatabaseExecutionContext) {
  val TYPE_ORDER = "order"
  val TYPE_CUSTOMER = "none"
  val TYPE_QUOTE = "quote"
  val TYPE_QUOTATION_REQUEST = "quotationRequest"

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
      val comments = DatabaseHelper.fetchMany(sql, params)(hydrateComment)(conn)
      val totalCount = DatabaseHelper.fetchColumn[Int]("SELECT FOUND_ROWS()")(conn)

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
}
