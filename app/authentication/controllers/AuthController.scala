package authentication.controllers

import javax.inject.Inject
import authentication.AuthService
import authentication.entities.User
import authentication.forms.{ChangePasswordForm, LoginForm}
import authentication.repositories.UserRepository
import cats.data.{EitherT, OptionT}
import cats.implicits._
import infra.actions.SessionAction
import infra.db.DBIO
import infra.responses.{ApiError, ApiResponse}
import inventory.util.DB
import inventory.validators.{DomainError, GenericError, InvalidPasswordResetToken}
import play.api.libs.json.Json
import play.api.mvc._
import scala.concurrent.ExecutionContext

class AuthController @Inject()(cc: ControllerComponents,
                               authAction: SessionAction,
                               authService: AuthService,
                               userRepo: UserRepository,
                               db: DB,
                              )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def logout = authAction {
    Ok.withNewSession
  }

  def changePassword = Action.async(parse.json) { req =>
    val error: DomainError = InvalidPasswordResetToken
    val program = for {
      form <- EitherT.fromOption[DBIO](req.body.asOpt[ChangePasswordForm], error)
      _ <- authService.changeUserPassword(form)
    } yield ()

    val result = program
      .map(_ => Ok(ApiResponse.empty.toJson))
      .valueOr {
        case InvalidPasswordResetToken => Forbidden(ApiError(InvalidPasswordResetToken.code, InvalidPasswordResetToken.errorMessage).toJson)
        case err => BadRequest(ApiError(err.code, err.errorMessage).toJson)
      }

    db.runAsync(result)
  }

  def resetPassword = Action.async(parse.json) { req =>
    val error: DomainError = GenericError
    val program = for {
      email <- EitherT.fromOption[DBIO]((req.body \ "email").asOpt[String], error)
      _ <- authService.resetUserPassword(email)
    } yield ()

    val response = Ok(ApiResponse.empty.toJson)
    val result: DBIO[Result] = program
      .map(_ => response)
      .valueOr(_ => response)

    db.runAsync(result)
  }

  def verifyPasswordToken(token: String) = Action.async {
    val program = for {
      prt <- OptionT(userRepo.getPasswordResetToken(token))
      _ <- OptionT.pure[DBIO](!prt.isExpired)
    } yield prt

    OptionT(db.runAsync(program.value))
      .map(prt => Ok(ApiResponse(prt).toJson))
      .getOrElse(NotFound("Invalid token"))
  }

  def check = authAction {
    Ok
  }

  def login = Action.async(parse.json) { request =>
    val program: OptionT[DBIO, User] = for {
      form <- OptionT.fromOption[DBIO](request.body.asOpt[LoginForm])
      user <- authService.verify(form)
    } yield user

    val result: DBIO[Result] = program
      .map(user => Ok(Json.toJson(ApiResponse(user))).withSession("user" -> user.username))
      .getOrElse(BadRequest("Invalid credentials"))

    db.runAsync(result)
  }
}
