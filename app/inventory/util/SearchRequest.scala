package inventory.util

import scala.util.Try

object SearchRequest {
  val SORT_ASC = "ASC";
  val SORT_DESC = "DESC";

  def fromQueryString(qs: Map[String, Seq[String]]) = {
    val limit = qs.get("limit").flatMap(seq => Try(seq(0).toInt).toOption).filter(_ >= 0)
    val offset = qs.get("offset").flatMap(seq => Try(seq(0).toInt.max(0)).toOption).getOrElse(0)
    val sort = qs.get("sort").map(sorts => {
      val pattern = """^(-)?([a-zA-Z0-9]+)$""".r

      sorts(0) match {
        case pattern("-", fieldName) => Some(("DESC", fieldName))
        case pattern(_, fieldName) => Some(("ASC", fieldName))
        case _ => None
      }
    }).flatten

    SearchRequest(
      filters = qs.map(tup => (tup._1 -> tup._2(0))),
      sortField = sort.map(_._2),
      sortOrder = sort.map(_._1).getOrElse(SORT_ASC),
      offset = offset,
      limit = limit
    )
  }
}

case class SearchRequest(
                          filters: Map[String, String] = Map(),
                          sortField: Option[String] = None,
                          sortOrder: String = SearchRequest.SORT_ASC,
                          offset: Int = 0,
                          limit: Option[Int] = None
                        ) {

  require(offset >= 0, "offset must be a natural number")
  require(limit match {
    case Some(i) if i < 0 => false
    case _ => true
  }, "limit must be a natural number")
}
