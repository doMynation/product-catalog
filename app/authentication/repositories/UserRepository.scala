package authentication.repositories

import java.sql.ResultSet
import javax.inject.Inject

import authentication.dtos.PasswordResetTokenDTO
import authentication.entities.{PasswordResetToken, User}
import infrastructure.DatabaseExecutionContext
import inventory.util.DatabaseHelper
import play.api.db.Database

import scala.concurrent.Future

final class UserRepository @Inject()(db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(id: Long): Future[Option[User]] = Future {
    val sql = "SELECT * FROM inv_users WHERE id = @userId"
    val fetch = DatabaseHelper.fetchOne(sql, Map("userId" -> id.toString))(hydrateUser) _

    db.withConnection(fetch)
  }

  def getByUsername(username: String): Future[Option[User]] = Future {
    val sql = "SELECT * FROM inv_users WHERE username = @username"
    val fetch = DatabaseHelper.fetchOne(sql, Map("username" -> username))(hydrateUser) _

    db.withConnection(fetch)
  }

  def getByEmail(email: String): Future[Option[User]] = Future {
    val sql = "SELECT * FROM inv_users WHERE email = @email"
    val fetch = DatabaseHelper.fetchOne(sql, Map("email" -> email))(hydrateUser) _

    db.withConnection(fetch)
  }

  def createUser(username: String, hashedPassword: String) = {
    val create = DatabaseHelper.insert("inv_users", Map(
      "full_name" -> "bob",
      "username" -> username,
      "password" -> hashedPassword,
    )) _

    db.withConnection(create)
  }

  def updateUserPassword(userId: Long, passwordHash: String): Unit = {
    val update = DatabaseHelper.executeUpdate("UPDATE inv_users SET password_hash = @passwordHash WHERE id = @userId", Map(
      "userId" -> userId.toString,
      "passwordHash" -> passwordHash
    )) _

    val _ = db.withConnection(update)
  }

  def getPasswordResetToken(token: String): Future[Option[PasswordResetToken]] = Future {
    val sql = "SELECT * FROM password_resets WHERE token = @token"
    val fetch = DatabaseHelper.fetchOne(sql, Map("token" -> token))(hydratePasswordResetToken) _

    db.withConnection(fetch)
  }

  def createPasswordResetToken(dto: PasswordResetTokenDTO) = {
    val create = DatabaseHelper.insert("password_resets", Map(
      "user_id" -> dto.userId.toString,
      "token" -> dto.token,
      "expiry_date" -> dto.expiresAt.toString,
    )) _

    db.withConnection(create)
  }

  def expireUserPasswordResetTokens(userId: Long): Unit = {
    val sql = "UPDATE password_resets SET expiry_date = NOW() WHERE expiry_date > NOW() AND user_id = @userId"
    val update = DatabaseHelper.executeUpdate(sql, Map("userId" -> userId.toString)) _

    val _ = db.withConnection(update)
  }

  private def hydrateUser(rs: ResultSet): User =
    User(
      rs.getLong("id"),
      rs.getString("email"),
      rs.getString("full_name"),
      rs.getString("username"),
      rs.getString("password_hash"),
      rs.getTimestamp("creation_date").toLocalDateTime,
    )

  private def hydratePasswordResetToken(rs: ResultSet): PasswordResetToken =
    PasswordResetToken(
      rs.getLong("id"),
      rs.getLong("user_id"),
      rs.getString("token"),
      rs.getTimestamp("expiry_date").toLocalDateTime,
      rs.getTimestamp("creation_date").toLocalDateTime
    )
}