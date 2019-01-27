package authentication

import java.security.SecureRandom
import java.time.LocalDateTime
import javax.inject.Inject

import akka.actor.ActorSystem
import authentication.dtos.PasswordResetTokenDTO
import authentication.entities.User
import authentication.forms.{ChangePasswordForm, LoginForm}
import authentication.repositories.UserRepository
import cats.effect.IO
import inventory.validators.{DomainError, InvalidPasswordResetToken, UserNotFound}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt
import util.Mailgun

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import cats.data._
import cats.implicits._

class AuthService @Inject()(userRepo: UserRepository, mailgun: Mailgun, actorSystem: ActorSystem)(implicit executionContext: ExecutionContext) {
  type ServiceResponse[T] = Future[Either[DomainError, T]]

  def verify(form: LoginForm): OptionT[IO, User] =
    for {
      _ <- OptionT.fromOption[IO](form.validate.toOption)
      user <- OptionT(IO.fromFuture(IO(userRepo.getByUsername(form.username))))
      isValid <- OptionT.liftF(verifyPassword(form.password, user.passwordHash))
      if isValid
    } yield user

  def changeUserPassword(form: ChangePasswordForm): ServiceResponse[Unit] = {
    val program = for {
      _ <- EitherT.fromEither[Future](form.validate) // Validate form
      prt <- EitherT.fromOptionF(userRepo.getPasswordResetToken(form.token), InvalidPasswordResetToken) // Find matching reset token
      _ <- EitherT.fromEither[Future](Either.cond(!prt.isExpired, prt, InvalidPasswordResetToken)) // Check token expiry
      user <- EitherT.fromOptionF(userRepo.get(prt.userId), InvalidPasswordResetToken) // Find matching user
      hashedPassword <- EitherT.rightT[Future, DomainError](BCrypt.hashpwUnsafe(form.password)) // Hash the new password
      _ <- EitherT.rightT[Future, DomainError](userRepo.updateUserPassword(user.id, hashedPassword.toString)) // Update the user password
      _ <- EitherT.rightT[Future, DomainError](userRepo.expireUserPasswordResetTokens(user.id)) // Consume the token
      _ <- EitherT.rightT[Future, DomainError](sendPasswordChangedConfirmationEmail(user)) // Send confirmation email
    } yield ()

    program.value
  }

  def resetUserPassword(email: String): ServiceResponse[Unit] = {
    val program = for {
      user <- EitherT.fromOptionF(userRepo.getByEmail(email), UserNotFound(email)) // Find the user
      prtDto <- EitherT.rightT[Future, DomainError](generatePasswordResetToken(user)) // Generate new token
      _ <- EitherT.rightT[Future, DomainError](userRepo.expireUserPasswordResetTokens(user.id)) // Expire all existing tokens
      _ <- EitherT.rightT[Future, DomainError](userRepo.createPasswordResetToken(prtDto)) // Save new token
      _ <- EitherT.rightT[Future, DomainError](sendPasswordResetConfirmationEmail(user, prtDto.token)) // Send confirmation email
    } yield ()

    program.value
  }

  private def verifyPassword(password: String, hash: String): IO[Boolean] =
    BCrypt.checkpwBool[IO](password, PasswordHash[BCrypt](hash))

  private def sendPasswordResetConfirmationEmail(user: User, token: String): Unit = {
    actorSystem.scheduler.scheduleOnce(5 seconds) {
      val link = s"http://localhost:3000/change-password?token=${token}"
      val body =
        s"""
           |Hello ${user.fullName},
           |
         |To reset your password, please click on the following link:
           |
        |${link}
      """.stripMargin

      mailgun.sendEmailSync(user.email, "Reset Your Password", body)
    }
  }

  private def sendPasswordChangedConfirmationEmail(user: User): Unit = {
    actorSystem.scheduler.scheduleOnce(5 seconds) {
      val body =
        s"""
           |Hello ${user.fullName},
           |
         |Your password was successfully changed.
      """.stripMargin

      mailgun.sendEmailSync(user.email, "Password Changed", body)
    }
  }

  private def generatePasswordResetToken(user: User): PasswordResetTokenDTO = {
    import tsec.common._
    import inventory.util.StringOps._

    val sr = new SecureRandom
    val bytes: Array[Byte] = Array.fill(32) {
      0
    }

    sr.nextBytes(bytes)

    val sha256Token = bytes.toHexString.sha256

    // Create a token that expires a day from now
    PasswordResetTokenDTO(
      user.id,
      sha256Token,
      LocalDateTime.now().plusDays(1)
    )
  }
}
