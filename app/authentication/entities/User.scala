package authentication.entities

import java.sql.ResultSet
import java.time.LocalDateTime

import play.api.libs.json._

object User {
  implicit def userHydrator(rs: ResultSet): User =
    User(
      rs.getLong("id"),
      rs.getString("email"),
      rs.getString("full_name"),
      rs.getString("username"),
      rs.getString("password_hash"),
      rs.getTimestamp("creation_date").toLocalDateTime,
    )

  implicit val userWrites: Writes[User] = new Writes[User] {
    override def writes(o: User): JsValue = JsObject(Seq(
      "id" -> JsNumber(o.id),
      "email" -> JsString(o.email),
      "username" -> JsString(o.username),
      "fullName" -> JsString(o.fullName),
    ))
  }
}

case class User(
                 id: Long,
                 email: String,
                 fullName: String,
                 username: String,
                 passwordHash: String,
                 createdAt: LocalDateTime = LocalDateTime.now
               )


