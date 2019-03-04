package shared

import cats.data.EitherT
import cats.effect.{IO, Resource}
import doobie._
import inventory.validators.DomainError

import scala.concurrent.Future

object Types {
  type Tx = Resource[IO, DataSourceTransactor[IO]]
  type ServiceResponse[A] = Future[Either[DomainError, A]]
  type ServiceResponse2[A] = EitherT[IO, DomainError, A]
  type Product = inventory.entities.Product
}
