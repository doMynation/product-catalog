package authentication.repositories

import javax.inject.Inject
import authentication.dtos.PasswordResetTokenDTO
import authentication.entities.{PasswordResetToken, User}
import infra.db.DBIO
import inventory.util.DB

final class UserRepository @Inject()(db: DB) {

  def getById(id: Long): DBIO[Option[User]] = DBIO { implicit conn =>
    val sql = "SELECT * FROM inv_users WHERE id = @userId"
    DB.fetchOne2[User](sql, Map("userId" -> id.toString))
  }

  def getByUsername(username: String): DBIO[Option[User]] = DBIO { implicit conn =>
    val sql = "SELECT * FROM inv_users WHERE username = @username"
    DB.fetchOne2[User](sql, Map("username" -> username))
  }

  def getByEmail(email: String): DBIO[Option[User]] = DBIO { implicit conn =>
    val sql = "SELECT * FROM inv_users WHERE email = @email"
    DB.fetchOne2[User](sql, Map("email" -> email))
  }

  def getPasswordResetToken(token: String): DBIO[Option[PasswordResetToken]] = DBIO { implicit conn =>
    val sql = "SELECT * FROM password_resets WHERE token = @token"
    DB.fetchOne2[PasswordResetToken](sql, Map("token" -> token))
  }

  def updateUserPassword(userId: Long, passwordHash: String): DBIO[Unit] = DBIO { conn =>
    DB.executeUpdate("UPDATE inv_users SET password_hash = @passwordHash WHERE id = @userId", Map(
      "userId" -> userId.toString,
      "passwordHash" -> passwordHash
    ))(conn)
  }

  def createPasswordResetToken(dto: PasswordResetTokenDTO): DBIO[Long] = DBIO { conn =>
    DB.insert("password_resets", Map(
      "user_id" -> dto.userId.toString,
      "token" -> dto.token,
      "expiry_date" -> dto.expiresAt.toString,
    ))(conn)
  }

  def expireUserPasswordResetTokens(userId: Long): DBIO[Unit] = DBIO { conn =>
    val sql = "UPDATE password_resets SET expiry_date = NOW() WHERE expiry_date > NOW() AND user_id = @userId"
    DB.executeUpdate(sql, Map("userId" -> userId.toString))(conn)
  }
}
