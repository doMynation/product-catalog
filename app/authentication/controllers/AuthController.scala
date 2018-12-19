package authentication.controllers

import javax.inject.Inject

import authentication.AuthService
import authentication.actions.AuthenticatedAction
import authentication.forms.LoginForm
import authentication.repositories.UserRepository
import cats.data.OptionT
import cats.effect.IO
import cats.implicits._
import infrastructure.ApiResponse
import play.api.libs.json.Json
import play.api.mvc._
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.ExecutionContext

class AuthController @Inject()(cc: ControllerComponents,
                               userRepo: UserRepository,
                               authAction: AuthenticatedAction,
                               authService: AuthService,
                              )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def logout = authAction {
    Ok.withNewSession
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
