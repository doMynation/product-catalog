import inventory.util.SearchRequest

import scala.collection.immutable.Queue
import scala.util.Try

object QueryTranslator {

  def translate(sql: String, params: Map[String, String] = Map()) = {
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

val sql = "SELECT * FROM table WHERE id=@id AND id2=@id AND name = @name and (lol=@lol) AND okay=@okay;"
val params = Map(
  ("id", "1234"),
  ("name", "Bob"),
  ("lol", "haha"),
  ("okay", "okay123"),
)
QueryTranslator.translate(sql, params)

val s = Set("hello", "bob")
val sortField = Some("bob1")

s("bob")

val thing = sortField.filter(s)

val maybeInt = Try("12".toInt).toOption

case class T1(offset: Int) {
  require (offset > 0, "shit")
}

val t = T1(39)
//val t2 = T1(-2)
val s = SearchRequest(offset = -2)


