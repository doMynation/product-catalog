package authentication.repositories

import javax.inject.Inject
import authentication.dtos.PasswordResetTokenDTO
import authentication.entities.{PasswordResetToken, User}
import infra.db.DBIO
import inventory.util.DB

final class UserRepository @Inject()(db: DB) {

  def getById(id: Long): DBIO[User] = {
    val sql = "SELECT * FROM inv_users WHERE id = @userId"
    DB.fetchOneDBIO[User](sql, Map("userId" -> id.toString)).map(_.get)
  }

  def getByUsername(username: String): DBIO[Option[User]] = {
    val sql = "SELECT * FROM inv_users WHERE username = @username"
    DB.fetchOneDBIO[User](sql, Map("username" -> username))
  }

  def getByEmail(email: String): DBIO[Option[User]] = {
    val sql = "SELECT * FROM inv_users WHERE email = @email"
    DB.fetchOneDBIO[User](sql, Map("email" -> email))
  }

  def getUserPrtsCount(userId: Long): DBIO[Int] = {
    val sql = "SELECT * FROM password_resets WHERE user_id = @userId"
    val query = DB.fetchManyDBIO[PasswordResetToken](sql, Map("userId" -> userId.toString))

    query.map(records => records.length)
  }

  def getPasswordResetToken(token: String): DBIO[Option[PasswordResetToken]] = {
    val sql = "SELECT * FROM password_resets WHERE token = @token"
    DB.fetchOneDBIO[PasswordResetToken](sql, Map("token" -> token))
  }

  def updateUserPassword(userId: Long, passwordHash: String): DBIO[Unit] = {
    val update = DB.executeUpdate("UPDATE inv_users SET password_hash = @passwordHash WHERE id = @userId", Map(
      "userId" -> userId.toString,
      "passwordHash" -> passwordHash
    )) _

    DBIO(update).map(_ => ())
  }

  def createPasswordResetToken(dto: PasswordResetTokenDTO): DBIO[Long] = {
    val create = DB.insert("password_resets", Map(
      "user_id" -> dto.userId.toString,
      "token" -> dto.token,
      "expiry_date" -> dto.expiresAt.toString,
    )) _

    DBIO(create)
  }

  def expireUserPasswordResetTokens(userId: Long): DBIO[Unit] = {
    val sql = "UPDATE password_resets SET expiry_date = NOW() WHERE expiry_date > NOW() AND user_id = @userId"
    val update = DB.executeUpdate(sql, Map("userId" -> userId.toString)) _

    DBIO(update).map(_ => ())
  }
}