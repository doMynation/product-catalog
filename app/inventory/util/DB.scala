package inventory.util

import java.sql._
import scala.collection.immutable.Queue

object DB {

  /**
    * Fetches the first record from the result of a database query.
    *
    * @param sql      An SQL statement
    * @param params   A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param hydrator A function that hydrates the resulting `ResultSet` into a `A`
    * @return A single record of type `A`
    */
  def fetchOneImpl[A](sql: String, params: Map[String, String])(implicit hydrator: ResultSet => A): Connection => Option[A] = {
    (connection: Connection) => {
      val rs = executeQuery(sql, params)(connection)

      if (rs.next) Some(hydrator(rs)) else None
    }
  }

  /**
    * Fetches the first record from the result of a database query.
    *
    * @param sql        An SQL statement
    * @param params     A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param hydrator   A function that hydrates the resulting `ResultSet` into a `A`
    * @param connection A database connection
    * @return A single record of type `A`
    */
  def fetchOne[A](sql: String, params: Map[String, String])(hydrator: ResultSet => A)(connection: Connection): Option[A] = {
    val rs = executeQuery(sql, params)(connection)

    if (rs.next) Some(hydrator(rs)) else None
  }

  /**
    * Fetches the first column of the first record from the result of a database query.
    *
    * @param sql        An SQL statement
    * @param params     A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param connection A database connection
    * @param extractor  An extractor for type `A`
    * @return A single value of type `A`
    */
  def fetchColumn[A](sql: String, params: Map[String, String] = Map())(connection: Connection)(implicit extractor: ResultSetFetchable[A]): Option[A] = {
    val rs = executeQuery(sql, params)(connection)

    if (rs.next) Some(extractor.get(rs))
    else None
  }

  /**
    * Fetches many records from the database.
    *
    * @param sql        An SQL statement
    * @param params     A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param hydrator   A function that hydrates the resulting `ResultSet` into a `A`
    * @param connection A database connection
    * @return A sequence of records of type `A`
    */
  def fetchMany[A](sql: String, params: Map[String, String])(hydrator: ResultSet => A)(connection: Connection): Seq[A] = {
    val rs = executeQuery(sql, params)(connection)
    var records = Queue[A]()

    while (rs.next) {
      records = records :+ hydrator(rs)
    }

    records
  }

  /**
    * Updates the supplied fields of records matching a given list of conditions.
    *
    * @param tableName       the name of the table
    * @param whereConditions a list of conditions using placeholders (e.g. "id = @id")
    * @param conditionParams a map of condition parameters to bind
    * @param fields          a map of fields to update
    * @param connection      a database connection
    * @return the number of affected records
    */
  def update(tableName: String, whereConditions: List[String], conditionParams: Map[String, String], fields: Map[String, String])(connection: Connection): Int = {
    require(!tableName.isEmpty, "tableName cannot be empty")
    require(!whereConditions.isEmpty, "conditions cannot be empty")
    require(!fields.isEmpty, "fields cannot be empty")

    val setClause = fields.keys.map(column => s"${column}=@${column}").mkString(", ")
    val sql = s"UPDATE ${tableName} SET ${setClause} WHERE ${whereConditions.mkString(" AND ")}"
    val params = fields ++ conditionParams

    executeUpdate(sql, params)(connection)
  }

  /**
    * Inserts a record in a given table.
    *
    * @param tableName  the name of the table
    * @param fields     a map of fields
    * @param connection a database connection
    * @return The ID of the inserted record.
    */
  def insert(tableName: String, fields: Map[String, String])(connection: Connection): Long = {
    require(!tableName.isEmpty, "tableName cannot be empty")
    require(!fields.isEmpty, "fields cannot be empty")

    val (columns, values) = fields.keys.foldLeft((Queue[String](), Queue[String]())) {
      (acc, columnName) =>
        acc match {
          case (columnsClause, valuesClause) => (columnsClause.enqueue(columnName), valuesClause.enqueue(s"@${columnName}"))
        }
    }

    val sql = s"INSERT INTO ${tableName} (${columns.mkString(",")}) VALUES (${values.mkString(",")})"

    executeInsert(sql, fields)(connection)
  }

  /**
    * Executes a read (SELECT) SQL statement.
    *
    * @param sql        An SQL statement
    * @param params     A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param connection A database connection
    * @return A `ResultSet` containing the result
    */
  def executeQuery(sql: String, params: Map[String, String])(connection: Connection): ResultSet = {
    val (translatedSql, orderedParams) = translate(sql, params)
    val stmt = prepareStatement(translatedSql, orderedParams)(connection)

    stmt.executeQuery
  }

  /**
    * Executes a write (INSERT, DELETE, UPDATE) SQL statement.
    *
    * @param sql        An SQL statement
    * @param params     A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param connection A database connection
    * @return An `Integer` representing the number of affected rows
    */
  def executeUpdate(sql: String, params: Map[String, String])(connection: Connection): Integer = {
    val (translatedSql, orderedParams) = translate(sql, params)
    val stmt = prepareStatement(translatedSql, orderedParams)(connection)

    stmt.executeUpdate
  }

  /**
    * Executes an INSERT SQL statement and returns the generated ID.
    *
    * @param sql        An SQL statement
    * @param params     A map of parameters, one for each `@var` placeholder in the SQL statement
    * @param connection A database connection
    * @return The ID of the inserted record.
    */
  def executeInsert(sql: String, params: Map[String, String])(connection: Connection): Long = {
    val (translatedSql, orderedParams) = translate(sql, params)
    val stmt = prepareStatement(translatedSql, orderedParams, true)(connection)

    stmt.executeUpdate
    val rs = stmt.getGeneratedKeys

    // @todo: Wrap this in Try{} and handle exceptions
    rs.next()
    rs.getLong(1)
  }

  /**
    * Translates an SQL statement with placeholders (e.g. `@var`) and a map of parameters to an SQL
    * statement with `?` and an ordered list of parameters.
    *
    * @param sql    An SQL statement
    * @param params A map of parameters, one for each `@var` placeholder in the SQL statement
    * @return A tuple containing the the translated SQL statement and ordered list of parameters
    */
  private def translate(sql: String, params: Map[String, String] = Map()): (String, Seq[String]) = {
    val regex = """(@([a-zA-Z0-9_]+))([\s\);$]?)""".r
    var orderedParams = Queue[String]()

    for (m <- regex.findAllMatchIn(sql)) {
      val paramKey = m.subgroups(1)

      params.get(paramKey).map(paramValue => {
        orderedParams = orderedParams :+ paramValue
      }).getOrElse(throw new RuntimeException(s"Parameter `$paramKey` not defined"))
    }

    val translatedSql = regex.replaceAllIn(sql, "?$3")

    (translatedSql, orderedParams)
  }

  /**
    * Prepares an SQL with a list of parameters to bind.
    *
    * @param sql        an SQL statement
    * @param params     a list of parameters to bind
    * @param connection the database connection
    * @return a `PreparedStatement`
    */
  private def prepareStatement(sql: String, params: Seq[String], returnInsertId: Boolean = false)(connection: Connection): PreparedStatement = {
    val stmt = connection.prepareStatement(sql,
      if (returnInsertId) Statement.RETURN_GENERATED_KEYS else Statement.NO_GENERATED_KEYS
    )

    var idx = 1
    params.foreach(param => {
      stmt.setObject(idx, param)
      idx = idx + 1
    })

    return stmt;
  }

  /**
    * Attempts to extract a value from a `ResultSet`.
    * If the value doesn't exist, `None` is returned. Otherwise, `Some(value)` is returned.
    *
    * @param columnName The column name
    * @param rs         The `ResultSet`
    * @tparam A The type of the underlying value
    * @return an Option[A]
    */
  def getNullable[A: NullableResultSetExtractor](columnName: String, rs: ResultSet): Option[A] = {
    implicitly[NullableResultSetExtractor[A]].getOpt(columnName, rs)
  }
}
