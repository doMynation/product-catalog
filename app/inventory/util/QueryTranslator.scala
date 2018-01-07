package inventory.util

import scala.collection.immutable.Queue

object QueryTranslator {

  def translate(sql: String, params: Map[String, String]): (String, Seq[String]) = {
    val regex = """(@([a-zA-Z0-9]+))([\s\);$]?)""".r
    var orderedParams = Queue[String]()

    for (m <- regex.findAllMatchIn(sql)) {
      val paramKey = m.subgroups(1)

      params.get(paramKey).map(paramValue => {
        orderedParams = orderedParams :+ paramValue
      }).getOrElse(throw new RuntimeException(s"Parameter `${paramKey}` not defined"))
    }

    val translatedSql = regex.replaceAllIn(sql, "?$3")

    (translatedSql, orderedParams)
  }
}
