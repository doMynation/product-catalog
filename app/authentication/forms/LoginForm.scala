package authentication.forms

import inventory.validators.{DomainError, GenericError}
import play.api.libs.json.{Json, Reads}

object LoginForm {
  implicit val reads: Reads[LoginForm] = Json.reads[LoginForm]
}

case class LoginForm(username: String, password: String) {
  def validate: Either[DomainError, LoginForm] = {
    for {
      _ <- Either.cond(username.trim.nonEmpty, username, GenericError)
      _ <- Either.cond(password.trim.nonEmpty, username, GenericError)
    } yield this
  }
}
