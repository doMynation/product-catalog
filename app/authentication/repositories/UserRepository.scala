package authentication.repositories

import java.sql.ResultSet
import javax.inject.Inject
import authentication.entities.User
import infrastructure.DatabaseExecutionContext
import inventory.util.DatabaseHelper
import play.api.db.Database
import scala.concurrent.Future

final class UserRepository @Inject()(db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(id: Long): Future[Option[User]] = Future {
    val sql = """SELECT * FROM inv_users WHERE id = @userId"""
    val fetch = DatabaseHelper.fetchOne(sql, Map("userId" -> id.toString))(hydrateUser) _

    db.withConnection(fetch)
  }

  def getByUsername(username: String): Future[Option[User]] = Future {
    val sql = """SELECT * FROM inv_users WHERE username = @username"""
    val fetch = DatabaseHelper.fetchOne(sql, Map("username" -> username))(hydrateUser) _

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

  private def hydrateUser(rs: ResultSet): User =
    User(
      rs.getLong("id"),
      rs.getString("full_name"),
      rs.getString("username"),
      rs.getString("password_hash"),
      rs.getTimestamp("creation_date").toLocalDateTime,
    )
}