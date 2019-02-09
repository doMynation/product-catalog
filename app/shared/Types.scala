package shared

import inventory.validators.DomainError
import scala.concurrent.Future

object Types {
  type ServiceResponse[A] = Future[Either[DomainError, A]]
  type Product = inventory.entities.Product
}
