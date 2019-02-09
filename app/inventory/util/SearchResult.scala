package inventory.util

import play.api.libs.json.{Json, Writes}

object SearchResult {
  implicit def searchResultWrites[A: Writes]: Writes[SearchResult[A]] = Json.writes[SearchResult[A]]
}

case class SearchResult[A](results: Seq[A], totalCount: Int)
