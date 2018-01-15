package inventory.util

import java.sql.ResultSet

trait ResultSetFetchable[T] {
  def get(rs: ResultSet): T
}

object ResultSetFetchable {
  implicit val stringFetchable = new ResultSetFetchable[String] {
    override def get(rs: ResultSet) = rs.getString(1)
  }

  implicit val intFetchable = new ResultSetFetchable[Int] {
    override def get(rs: ResultSet) = rs.getInt(1)
  }
}


