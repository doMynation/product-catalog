package authentication

import inventory.entities.Store
import play.api.mvc._

case class ApiRequest[A](store: Store, request: Request[A]) extends WrappedRequest[A](request)
