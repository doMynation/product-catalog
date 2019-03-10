package shared

import cats.data.EitherT
import cats.effect.{IO, Resource}
import doobie._
import inventory.validators.DomainError

object Types {
  type Tx = Resource[IO, DataSourceTransactor[IO]]

  type ServiceResponse[A] = EitherT[IO, DomainError, A]
  type Product = inventory.entities.Product
}
