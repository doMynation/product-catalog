package authentication.repositories

import authentication.dtos.PasswordResetTokenDTO
import javax.inject
import authentication.entities.{PasswordResetToken, User}
import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._
import utils.imports.implicits._
import infra.DatabaseExecutionContext
import shared.Types.Tx

final class UserRepository @inject.Inject()(res: Tx)(implicit ec: DatabaseExecutionContext) {

  def getById(id: Long): IO[Option[User]] = {
    val sql = sql"SELECT id, email, full_name, username, password_hash, creation_date FROM inv_users WHERE id = $id"
    val query = sql.query[User].option

    res.use(tx => query.transact(tx))
  }

  def getByUsername(username: String): IO[Option[User]] = {
    val sql = sql"SELECT id, email, full_name, username, password_hash, creation_date FROM inv_users WHERE username = $username"
    val query = sql.query[User].option

    res.use(tx => query.transact(tx))
  }

  def getByEmail(email: String): IO[Option[User]] = {
    val sql = sql"SELECT id, email, full_name, username, password_hash, creation_date FROM inv_users WHERE email = $email"
    val query = sql.query[User].option

    res.use(tx => query.transact(tx))
  }

  def getPasswordResetToken(token: String): IO[Option[PasswordResetToken]] = {
    val sql = sql"SELECT id, user_id, token, expiry_date, creation_date FROM password_resets WHERE token = $token"
    val query = sql.query[PasswordResetToken].option

    res.use(tx => query.transact(tx))
  }

  def updateUserPassword(userId: Long, passwordHash: String): IO[Unit] = {
    val sql = sql"UPDATE inv_users SET password_hash = $passwordHash WHERE id = $userId"
    val query = sql.update.run.map(_ => ())

    res.use(tx => query.transact(tx))
  }

  def createPasswordResetToken(dto: PasswordResetTokenDTO): IO[Long] = {
    val sql =
      sql"""
           |INSERT INTO password_resets (user_id, token, expiry_date)
           |VALUES (${dto.userId}, ${dto.token}, ${dto.expiresAt.toString})
           |""".stripMargin

    val query = sql.update.withUniqueGeneratedKeys[Long]("id")

    res.use(tx => query.transact(tx))
  }

  def expireUserPasswordResetTokens(userId: Long): IO[Unit] = {
    val sql = sql"UPDATE password_resets SET expiry_date = NOW() WHERE expiry_date > NOW() AND user_id = $userId"
    val query = sql.update.run.map(_ => ())

    res.use(tx => query.transact(tx))
  }

}
