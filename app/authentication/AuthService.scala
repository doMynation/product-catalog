package authentication

import java.security.SecureRandom
import java.time.LocalDateTime
import javax.inject.Inject
import akka.actor.ActorSystem
import authentication.dtos.PasswordResetTokenDTO
import authentication.entities.User
import authentication.forms.{ChangePasswordForm, LoginForm}
import authentication.repositories.{UserRepositoryDoobie}
import inventory.validators.{DomainError, InvalidPasswordResetToken, UserNotFound}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt
import utils.Mailgun
import scala.concurrent.{ExecutionContext}
import scala.concurrent.duration._
import cats.data._
import cats.effect.IO
import cats.implicits._

final class AuthService @Inject()(userRepo: UserRepositoryDoobie, mailgun: Mailgun, actorSystem: ActorSystem)(implicit ec: ExecutionContext) {
  def verify(form: LoginForm): OptionT[IO, User] =
    for {
      _ <- OptionT.fromOption[IO](form.validate.toOption)
      user <- OptionT(userRepo.getByUsername(form.username))
      isValid <- OptionT.liftF(verifyPassword(form.password, user.passwordHash))
      if isValid
    } yield user

  def changeUserPassword(form: ChangePasswordForm): EitherT[IO, DomainError, Unit] =
    for {
      _ <- EitherT.fromEither[IO](form.validate) // 1) Validate form
      prt <- EitherT.fromOptionF(userRepo.getPasswordResetToken(form.token), InvalidPasswordResetToken) // 2) Find matching reset token
      _ <- EitherT.fromEither[IO](Either.cond(!prt.isExpired, prt, InvalidPasswordResetToken)) // 3) Check token expiry
      user <- EitherT.fromOptionF(userRepo.getById(prt.userId), InvalidPasswordResetToken) // 4) Find matching user
      hashedPassword <- EitherT.right[DomainError](BCrypt.hashpw[IO](form.password)) // 5) Hash the new password
      _ <- EitherT.right[DomainError](userRepo.updateUserPassword(user.id, hashedPassword.toString)) // 6) Update the user password
      _ <- EitherT.right[DomainError](userRepo.expireUserPasswordResetTokens(user.id)) // 7) Consume the token
      _ <- EitherT.right[DomainError](IO.shift(ec)) // 9) Shift to main thread
      _ <- EitherT.rightT[IO, DomainError](sendPasswordChangedConfirmationEmail(user)) // 10) Send confirmation email
    } yield ()

  def resetUserPassword(email: String): EitherT[IO, DomainError, Unit] =
    for {
      user <- EitherT.fromOptionF(userRepo.getByEmail(email), UserNotFound(email)) // Find the user
      prtDto <- EitherT.rightT[IO, DomainError](generatePasswordResetToken(user)) // Generate new token
      _ <- EitherT.right[DomainError](userRepo.expireUserPasswordResetTokens(user.id)) // Expire all existing tokens
      _ <- EitherT.right[DomainError](userRepo.createPasswordResetToken(prtDto)) // Save new token
      _ <- EitherT.rightT[IO, DomainError](sendPasswordResetConfirmationEmail(user, prtDto.token)) // Send confirmation email
    } yield ()

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
