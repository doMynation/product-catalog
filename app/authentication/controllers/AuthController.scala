package authentication.controllers

import javax.inject.Inject

import authentication.AuthService
import authentication.actions.AuthenticatedAction
import authentication.forms.{ChangePasswordForm, LoginForm}
import authentication.repositories.UserRepository
import cats.data.OptionT
import cats.effect.IO
import cats.implicits._
import infrastructure.{ApiError, ApiResponse}
import inventory.validators.{DomainError, InvalidPasswordResetToken}
import play.api.libs.json.{JsError, JsSuccess, Json}
import play.api.mvc._
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.{ExecutionContext, Future}

class AuthController @Inject()(cc: ControllerComponents,
                               userRepo: UserRepository,
                               authAction: AuthenticatedAction,
                               authService: AuthService,
                              )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def logout = authAction {
    Ok.withNewSession
  }

  def changePassword = Action.async(parse.json) { req =>
    val of = req.body.validate[ChangePasswordForm]

    of match {
      case form: JsSuccess[ChangePasswordForm] =>
        val result: Future[Either[DomainError, Unit]] = authService.changeUserPassword(form.get)

        result.map {
          case Right(_) => Ok(ApiResponse.empty.toJson)
          case Left(InvalidPasswordResetToken) => Forbidden(ApiError(InvalidPasswordResetToken.code, InvalidPasswordResetToken.errorMessage).toJson)
          case Left(err) => BadRequest(ApiError(err.code, err.errorMessage).toJson)
        }
      case e: JsError =>
        Future.successful(BadRequest("Invalid request"))
    }
  }

  def resetPassword = Action.async(parse.json) { req =>
    val eo = (req.body \ "email").asOpt[String]

    if (eo.isEmpty) Future.successful(BadRequest("Invalid request"))

    val result = authService.resetUserPassword(eo.get)

    result.map {
      case Right(_) => Ok(ApiResponse.empty.toJson)
      case Left(_) => Ok(ApiResponse.empty.toJson) // Return a success even when the email wasn't found
    }
  }

  def verifyPasswordToken(token: String) = Action.async {
    val fop = OptionT(userRepo.getPasswordResetToken(token))

    fop
      .map(prt => {
        if (prt.isExpired) NotFound("Invalid Token")
        else Ok(ApiResponse(prt).toJson)
      })
      .getOrElse(NotFound("Invalid token"))
  }

  def signup(username: String, password: String) = Action {
    val userId = userRepo.createUser(username, BCrypt.hashpwUnsafe(password))

    Ok(s"Created $userId")
  }

  def check = authAction {
    Ok
  }

  def login = Action.async(parse.json) { request =>
    val validateCredentials = for {
      form <- OptionT.fromOption[IO](request.body.asOpt[LoginForm])
      user <- authService.verify(form)
    } yield user

    validateCredentials
      .map(user =>
        Ok(Json.toJson(ApiResponse(user)))
          .withSession("user" -> user.username)
      )
      .getOrElse(BadRequest("Nope"))
      .unsafeToFuture
  }
}
