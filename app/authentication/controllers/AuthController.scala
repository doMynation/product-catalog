package authentication.controllers

import authentication.AuthService
import authentication.entities.User
import authentication.forms.{ChangePasswordForm, LoginForm}
import authentication.repositories.{UserRepository}
import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.implicits._
import infra.actions.SessionAction
import infra.responses.{ApiError, ApiResponse}
import inventory.validators.{DomainError, GenericError, InvalidPasswordResetToken}
import javax.inject.Inject
import play.api.libs.json.Json
import play.api.mvc._
import scala.concurrent.ExecutionContext

class AuthController @Inject()(cc: ControllerComponents,
                               authAction: SessionAction,
                               authService: AuthService,
                               doobieRepo: UserRepository,
                              ) extends AbstractController(cc) {

  def logout = authAction {
    Ok.withNewSession
  }

  def changePassword = Action.async(parse.json) { req =>
    val error: DomainError = InvalidPasswordResetToken
    val program = for {
      form <- EitherT.fromOption[IO](req.body.asOpt[ChangePasswordForm], error)
      _ <- authService.changeUserPassword(form)
    } yield ()

    val result = program
      .map(_ => Ok(ApiResponse.empty.toJson))
      .valueOr {
        case InvalidPasswordResetToken => Forbidden(ApiError(InvalidPasswordResetToken.code, InvalidPasswordResetToken.errorMessage).toJson)
        case err => BadRequest(ApiError(err.code, err.errorMessage).toJson)
      }

    result.unsafeToFuture
  }

  def resetPassword = Action.async(parse.json) { req =>
    val email = (req.body \ "email").asOpt[String]
    val error: DomainError = GenericError
    val response = Ok(ApiResponse.empty.toJson)

    val program = for {
      email <- EitherT.fromOption[IO](email, error)
      _ <- authService.resetUserPassword(email)
    } yield ()

    program
      .map(_ => response)
      .valueOr(_ => response)
      .unsafeToFuture
  }

  def verifyPasswordToken(token: String) = Action.async {
    val program = for {
      prt <- OptionT(doobieRepo.getPasswordResetToken(token))
      if !prt.isExpired
    } yield prt

    program
      .map(prt => Ok(ApiResponse(prt).toJson))
      .getOrElse(NotFound("Invalid token"))
      .unsafeToFuture
  }

  def check = authAction {
    Ok
  }

  def login = Action.async(parse.json) { request =>
    val program: OptionT[IO, User] = for {
      form <- OptionT.fromOption[IO](request.body.asOpt[LoginForm])
      user <- authService.verify(form)
    } yield user

    val result: IO[Result] = program
      .map(user => Ok(Json.toJson(ApiResponse(user))).withSession("user" -> user.username))
      .getOrElse(BadRequest("Invalid credentials"))

    result.unsafeToFuture
  }
}
