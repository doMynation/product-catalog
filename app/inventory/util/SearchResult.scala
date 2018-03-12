package inventory.util

import play.api.libs.json.{Json, Writes}

object SearchResult {
  implicit def searchResultWrites[T: Writes]: Writes[SearchResult[T]] = Json.writes[SearchResult[T]]
}

case class SearchResult[T](results: Seq[T], totalCount: Int)
