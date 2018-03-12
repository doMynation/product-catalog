package inventory.util

import java.sql.ResultSet
import java.time.LocalDateTime

trait NullableResultSetExtractor[T] {
  def getOpt(columnName: String, rs: ResultSet): Option[T]
}

object NullableResultSetExtractor {
  implicit val stringNullableExtractor = new NullableResultSetExtractor[String] {
    override def getOpt(columnName: String, rs: ResultSet): Option[String] = {
      val v = rs.getString(columnName)

      if (rs.wasNull()) None
      else Some(v)
    }
  }

  implicit val intNullableExtractor = new NullableResultSetExtractor[Int] {
    override def getOpt(columnName: String, rs: ResultSet): Option[Int] = {
      val v = rs.getInt(columnName)

      if (rs.wasNull()) None
      else Some(v)
    }
  }

  implicit val longNullableExtractor = new NullableResultSetExtractor[Long] {
    override def getOpt(columnName: String, rs: ResultSet): Option[Long] = {
      val v = rs.getLong(columnName)

      if (rs.wasNull()) None
      else Some(v)
    }
  }

  implicit val booleanNullableExtractor = new NullableResultSetExtractor[Boolean] {
    override def getOpt(columnName: String, rs: ResultSet): Option[Boolean] = {
      val v = rs.getBoolean(columnName)

      if (rs.wasNull()) None
      else Some(v)
    }
  }

  implicit val dateTimeNullableExtractor = new NullableResultSetExtractor[LocalDateTime] {
    override def getOpt(columnName: String, rs: ResultSet): Option[LocalDateTime] = {
      val v = rs.getTimestamp(columnName)

      if (rs.wasNull()) None
      else Some(v.toLocalDateTime)
    }
  }

  implicit val bigDecimalNullableExtractor = new NullableResultSetExtractor[BigDecimal] {
    override def getOpt(columnName: String, rs: ResultSet): Option[BigDecimal] = {
      val v = rs.getBigDecimal(columnName)

      if (rs.wasNull()) None
      else Some(BigDecimal(v))
    }
  }
}
