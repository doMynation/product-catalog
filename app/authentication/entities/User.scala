package authentication.entities

import java.time.LocalDateTime
import play.api.libs.json._

object User {
  implicit val writes: Writes[User] = new Writes[User] {
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

