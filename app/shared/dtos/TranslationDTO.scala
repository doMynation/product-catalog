package shared.dtos

import play.api.libs.json.Reads
import play.api.libs.json._
import play.api.libs.functional.syntax._

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
                           isDefault: Boolean = false)
