package infra.db

import java.sql.Connection

import cats.{Monad, StackSafeMonad}

object DBIO {
  def apply[A](run: Connection => A): DBIO[A] = new DBIO(run)

  implicit val dbioMonad: Monad[DBIO] = new Monad[DBIO] with StackSafeMonad[DBIO] {
    override def flatMap[A, B](fa: DBIO[A])(f: A => DBIO[B]): DBIO[B] = fa.flatMap(f)

    override def pure[A](x: A): DBIO[A] = DBIO(_ => x)
  }
}

final class DBIO[A](val run: Connection => A) {
  def map[B](f: A => B): DBIO[B] =
    new DBIO(f compose run)

  def flatMap[B](f: A => DBIO[B]): DBIO[B] =
    new DBIO((c: Connection) => f(run(c)).run(c))
}
