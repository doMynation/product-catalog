package authentication.repositories

import javax.inject.Inject
import authentication.dtos.PasswordResetTokenDTO
import authentication.entities.{PasswordResetToken, User}
import infrastructure.DatabaseExecutionContext
import inventory.util.DB
import play.api.db.Database
import scala.concurrent.Future

final class UserRepository @Inject()(db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(id: Long): Future[Option[User]] = Future {
    val sql = "SELECT * FROM inv_users WHERE id = @userId"
    val fetch = DB.fetchOneImpl[User](sql, Map("userId" -> id.toString))

    db.withConnection(fetch)
  }

  def getByUsername(username: String): Future[Option[User]] = Future {
    val sql = "SELECT * FROM inv_users WHERE username = @username"
    val fetch = DB.fetchOneImpl[User](sql, Map("username" -> username))

    db.withConnection(fetch)
  }

  def getByEmail(email: String): Future[Option[User]] = Future {
    val sql = "SELECT * FROM inv_users WHERE email = @email"
    val fetch = DB.fetchOneImpl[User](sql, Map("email" -> email))

    db.withConnection(fetch)
  }

  def createUser(username: String, hashedPassword: String): Long = {
    val create = DB.insert("inv_users", Map(
      "full_name" -> "bob",
      "username" -> username,
      "password" -> hashedPassword,
    )) _

    db.withConnection(create)
  }

  def updateUserPassword(userId: Long, passwordHash: String): Unit = {
    val update = DB.executeUpdate("UPDATE inv_users SET password_hash = @passwordHash WHERE id = @userId", Map(
      "userId" -> userId.toString,
      "passwordHash" -> passwordHash
    )) _

    val _ = db.withConnection(update)
  }

  def getPasswordResetToken(token: String): Future[Option[PasswordResetToken]] = Future {
    val sql = "SELECT * FROM password_resets WHERE token = @token"
    val fetch = DB.fetchOneImpl[PasswordResetToken](sql, Map("token" -> token))

    db.withConnection(fetch)
  }

  def createPasswordResetToken(dto: PasswordResetTokenDTO): Long = {
    val create = DB.insert("password_resets", Map(
      "user_id" -> dto.userId.toString,
      "token" -> dto.token,
      "expiry_date" -> dto.expiresAt.toString,
    )) _

    db.withConnection(create)
  }

  def expireUserPasswordResetTokens(userId: Long): Unit = {
    val sql = "UPDATE password_resets SET expiry_date = NOW() WHERE expiry_date > NOW() AND user_id = @userId"
    val update = DB.executeUpdate(sql, Map("userId" -> userId.toString)) _

    db.withConnection(update)

    ()
  }
}