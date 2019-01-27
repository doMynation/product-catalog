package inventory.validators

import shared.dtos.TranslationDTO
import cats.instances._
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._

object CommonValidator {
  def validateTranslations(translations: List[TranslationDTO]): Either[DomainError, Seq[TranslationDTO]] = {
    for {
      _ <- translations.traverse(_.validate)
      _ <- Either.cond(translations.count(_.isDefault) == 1, translations, InvalidTranslations)
    } yield translations
  }
}
