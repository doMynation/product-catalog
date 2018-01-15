package inventory.util

import java.sql.{Connection, ResultSet}
import scala.collection.immutable.Queue

object DatabaseHelper {

  /**
    * Fetches the first record from the result of a database query.
    *
    * @param sql      An SQL statement
    * @param params   A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param hydrator A function that hydrates the resulting `ResultSet` into a `T`
    * @param conn     A database connection
    * @return A single record of type `T`
    */
  def fetchOne[T](sql: String, params: Map[String, String])(hydrator: ResultSet => T)(implicit conn: Connection): Option[T] = {
    val rs = executeQuery(sql, params)

    if (rs.next) Some(hydrator(rs)) else None
  }

  /**
    * Fetches the first column of the first record from the result of a database query.
    *
    * @param sql       An SQL statement
    * @param params    A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param conn      A database connection
    * @param extractor An extractor for type `T`
    * @return A single value of type `T`
    */
  def fetchColumn[T](sql: String, params: Map[String, String])(implicit conn: Connection, extractor: ResultSetFetchable[T]): Option[T] = {
    val rs = executeQuery(sql, params)

    if (rs.next) Some(extractor.get(rs))
    else None
  }

  /**
    * Fetches many records from the database.
    *
    * @param sql      An SQL statement
    * @param params   A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param hydrator A function that hydrates the resulting `ResultSet` into a `T`
    * @param conn     A database connection
    * @return A sequence of records of type `T`
    */
  def fetchMany[T](sql: String, params: Map[String, String])(hydrator: ResultSet => T)(implicit conn: Connection): Seq[T] = {
    val rs = executeQuery(sql, params)
    var records = Queue[T]()

    while (rs.next) {
      records = records :+ hydrator(rs)
    }

    records
  }

  /**
    * Translates an SQL statement with placeholders (e.g. `@var`) and a map of parameters to an SQL
    * statement with `?` and an ordered list of parameters.
    *
    * @param sql    An SQL statement
    * @param params A map of parameters, one for each `@var` placeholder in the SQL statement
    * @return A tuple containing the the translated SQL statement and ordered list of parameters
    */
  def translate(sql: String, params: Map[String, String] = Map()): (String, Seq[String]) = {
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

  /**
    * Executes an SQL statement.
    *
    * @param sql    An SQL statement
    * @param params A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param conn   A database connection
    * @return A `ResultSet` containing the result
    */
  def executeQuery(sql: String, params: Map[String, String])(implicit conn: Connection): ResultSet = {
    // Translate the query
    val (translatedSql, orderedParams) = translate(sql, params)

    // Prepare the statement and bind all parameters
    val stmt = conn.prepareStatement(translatedSql)

    var idx = 1
    orderedParams.foreach(param => {
      stmt.setObject(idx, param)
      idx = idx + 1
    })

    stmt.executeQuery
  }
}
