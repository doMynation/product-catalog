package authentication.entities

import java.time.LocalDateTime

import play.api.libs.json.{Json, Writes}

object PasswordResetToken {
  implicit val passwordResetTokenWrites: Writes[PasswordResetToken] = Json.writes[PasswordResetToken]
}

case class PasswordResetToken(id: Long, userId: Long, token: String, expiresAt: LocalDateTime, createdAt: LocalDateTime) {
  def isExpired: Boolean = LocalDateTime.now.isAfter(expiresAt)
}
