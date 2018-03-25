package inventory.util

import java.sql.ResultSet

trait ResultSetFetchable[T] {
  def get(rs: ResultSet): T
}

object ResultSetFetchable {
  implicit val stringFetchable: ResultSetFetchable[String] = (rs: ResultSet) => rs.getString(1)
  implicit val intFetchable: ResultSetFetchable[Int] = (rs: ResultSet) => rs.getInt(1)
  implicit val bigDecimalFetchable: ResultSetFetchable[BigDecimal] = (rs: ResultSet) => BigDecimal(rs.getBigDecimal(1))
}


