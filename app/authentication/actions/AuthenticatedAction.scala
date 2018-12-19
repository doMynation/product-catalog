package authentication.actions

import javax.inject.Inject
import authentication.SecureRequest
import authentication.entities.User
import authentication.repositories.UserRepository
import cats.data.OptionT
import cats.implicits._
import play.api.mvc.Results._
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}

class AuthenticatedAction @Inject()(val parser: BodyParsers.Default, userRepo: UserRepository)
                                   (implicit val executionContext: ExecutionContext) extends ActionBuilder[SecureRequest, AnyContent] {

  override def invokeBlock[A](request: Request[A], block: SecureRequest[A] => Future[Result]) = {
    // Check if the user has an active session
    val userHasSession: OptionT[Future, User] = for {
      username <- OptionT.fromOption[Future](request.session.get("user"))
      user <- OptionT(userRepo.getByUsername(username))
    } yield user

    userHasSession
      .map(user => block(SecureRequest(user, request)))
      .getOrElse(Future.successful(Unauthorized("No Session Found")))
      .flatten
  }
}
