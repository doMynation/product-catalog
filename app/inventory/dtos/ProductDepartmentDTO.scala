package inventory.dtos

import inventory.validators._
import play.api.libs.json.Reads
import play.api.libs.json._
import play.api.libs.functional.syntax._
import shared.dtos.TranslationDTO

object ProductDepartmentDTO {
  implicit val reads: Reads[ProductDepartmentDTO] = (
    (__ \ "code").read[String] and
      (__ \ "translations").read[List[TranslationDTO]]
    ) (ProductDepartmentDTO.apply _)
}

case class ProductDepartmentDTO(code: String, translations: List[TranslationDTO] = List()) {
  def validate: Either[DomainError, ProductDepartmentDTO] =
    for {
      _ <- Either.cond(code.nonEmpty, code, InvalidCode)
      _ <- CommonValidator.validateTranslations(translations.toList)
    } yield this
}


