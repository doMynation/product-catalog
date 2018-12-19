package authentication

import javax.inject.Inject
import authentication.entities.User
import authentication.forms.LoginForm
import authentication.repositories.UserRepository
import cats.data.OptionT
import cats.effect.IO
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

class AuthService @Inject()(userRepo: UserRepository) {

  def verify(form: LoginForm): OptionT[IO, User] =
    for {
      _ <- OptionT.fromOption[IO](form.validate.toOption)
      user <- OptionT(IO.fromFuture(IO(userRepo.getByUsername(form.username))))
      isValid <- OptionT.liftF(verifyPassword(form.password, user.passwordHash))
      if isValid
    } yield user

  private def verifyPassword(password: String, hash: String): IO[Boolean] =
    BCrypt.checkpwBool[IO](password, PasswordHash[BCrypt](hash))

}
