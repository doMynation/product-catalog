package shared.dtos

import inventory.validators.{DomainError, InvalidLanguage, InvalidName}
import play.api.libs.json.Reads
import play.api.libs.json._
import play.api.libs.functional.syntax._
import shared.entities.Lang

object TranslationDTO {
  implicit val dtoReads: Reads[TranslationDTO] = (
    (__ \ "lang").read[String] and
      (__ \ "name").read[String] and
      (__ \ "shortDescription").read[String] and
      (__ \ "longDescription").read[String] and
      (__ \ "isDefault").read[Boolean]
    ) (TranslationDTO.apply _)
}

case class TranslationDTO(
                           lang: String,
                           name: String,
                           shortDescription: String = "",
                           longDescription: String = "",
                           isDefault: Boolean = false
                         ) {
  def validate: Either[DomainError, TranslationDTO] =
    for {
      _ <- Either.cond(
        Set(Lang.EN, Lang.FR, Lang.ES).contains(lang),
        lang,
        InvalidLanguage(lang)
      )
      _ <- Either.cond(name.nonEmpty, name, InvalidName)
    } yield this
}
