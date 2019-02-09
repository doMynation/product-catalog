package infra.requests

import authentication.entities.User
import play.api.mvc._

case class SessionRequest[A](user: User, request: Request[A]) extends WrappedRequest[A](request)
