package infra.actions

import javax.inject.Inject
import authentication.entities.User
import authentication.repositories.UserRepositoryDoobie
import cats.data.OptionT
import cats.effect.IO
import cats.implicits._
import infra.requests.SessionRequest
import play.api.mvc.Results._
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}

class SessionAction @Inject()(val parser: BodyParsers.Default, userRepo: UserRepositoryDoobie)
                             (implicit val executionContext: ExecutionContext) extends ActionBuilder[SessionRequest, AnyContent] {

  override def invokeBlock[A](request: Request[A], block: SessionRequest[A] => Future[Result]) = {
    // Check if the user has an active session
    val program: OptionT[IO, User] = for {
      username <- OptionT.fromOption[IO](request.session.get("user"))
      user <- OptionT(userRepo.getByUsername(username))
    } yield user

    program
      .map(user => block(SessionRequest(user, request)))
      .getOrElse(Future.successful(Unauthorized("No Session Found")))
      .unsafeToFuture
      .flatten
  }
}
