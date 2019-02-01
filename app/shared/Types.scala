package shared

import inventory.validators.DomainError
import scala.concurrent.Future

object Types {
  type ServiceResponse[T] = Future[Either[DomainError, T]]
  type Product = inventory.entities.Product
}
