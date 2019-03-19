package shared.repositories

import cats.effect.IO
import doobie._
import doobie.implicits._
import javax.inject.Inject
import inventory.util.{SearchRequest, SearchResult}
import shared.entities.Comment
import utils.SolariusDB
import utils.imports.implicits._

final class CommentRepository @Inject()(solarius: SolariusDB) {
  val TYPE_ORDER = "order"
  val TYPE_CUSTOMER = "none"
  val TYPE_QUOTE = "quote"
  val TYPE_QUOTATION_REQUEST = "quotationRequest"

  def searchIO(sr: SearchRequest): IO[SearchResult[Comment]] =
    solarius.run(Queries.search(sr))

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
