package authentication

import authentication.entities.User
import play.api.mvc._

case class SecureRequest[A](user: User, request: Request[A]) extends WrappedRequest[A](request)
