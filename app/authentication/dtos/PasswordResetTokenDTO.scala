package authentication.dtos

import java.time.LocalDateTime

case class PasswordResetTokenDTO(userId: Long, token: String, expiresAt: LocalDateTime)
