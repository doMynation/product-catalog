package authentication.forms

import inventory.validators._
import play.api.libs.json.{Json, Reads}

object ChangePasswordForm {
  implicit val reads: Reads[ChangePasswordForm] = Json.reads[ChangePasswordForm]
}

case class ChangePasswordForm(token: String, password: String, passwordConfirmation: String) {
  def validate: Either[DomainError, ChangePasswordForm] = {
    val passwordRegex = "^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[#?!@$%^&*-]).{8,}$"

    for {
      _ <- Either.cond(token.trim.nonEmpty, true, InvalidPasswordResetToken)
      _ <- Either.cond(password.trim.nonEmpty, true, InvalidPassword)
      _ <- Either.cond(passwordConfirmation.trim.nonEmpty, true, InvalidPassword)
      _ <- Either.cond(password == passwordConfirmation, true, InvalidPassword)
      _ <- Either.cond(password.matches(passwordRegex), true, InvalidPassword)
    } yield this
  }
}
