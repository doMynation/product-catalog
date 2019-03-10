package utils

import doobie._
import doobie.implicits._
import cats.effect.{IO, Resource}

final class SolariusDB(res: Resource[IO, DataSourceTransactor[IO]]) {
  def run[A](query: ConnectionIO[A]): IO[A] = res.use(query.transact(_))
}
