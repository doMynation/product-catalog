package authentication.entities

import java.sql.ResultSet
import java.time.LocalDateTime

import play.api.libs.json.{Json, Writes}

object PasswordResetToken {
  implicit def prtHydrator(rs: ResultSet): PasswordResetToken =
    PasswordResetToken(
      rs.getLong("id"),
      rs.getLong("user_id"),
      rs.getString("token"),
      rs.getTimestamp("expiry_date").toLocalDateTime,
      rs.getTimestamp("creation_date").toLocalDateTime
    )

  implicit val passwordResetTokenWrites: Writes[PasswordResetToken] = Json.writes[PasswordResetToken]
}

case class PasswordResetToken(id: Long, userId: Long, token: String, expiresAt: LocalDateTime, createdAt: LocalDateTime) {
  def isExpired: Boolean = LocalDateTime.now.isAfter(expiresAt)
}
