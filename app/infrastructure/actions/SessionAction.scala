package infrastructure.actions

import javax.inject.Inject

import authentication.entities.User
import authentication.repositories.UserRepository
import cats.data.OptionT
import cats.implicits._
import infrastructure.requests.SessionRequest
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class SessionAction @Inject()(val parser: BodyParsers.Default, userRepo: UserRepository)
                             (implicit val executionContext: ExecutionContext) extends ActionBuilder[SessionRequest, AnyContent] {

  override def invokeBlock[A](request: Request[A], block: SessionRequest[A] => Future[Result]) = {
    // Check if the user has an active session
    val userHasSession: OptionT[Future, User] = for {
      username <- OptionT.fromOption[Future](request.session.get("user"))
      user <- OptionT(userRepo.getByUsername(username))
    } yield user

    userHasSession
      .map(user => block(SessionRequest(user, request)))
      .getOrElse(Future.successful(Unauthorized("No Session Found")))
      .flatten
  }
}
