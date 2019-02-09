package inventory.entities

import play.api.libs.json.{Json, Writes}
import shared.dtos.TranslationDTO
import utils.DTOMappable

object Translation extends DTOMappable[Translation, TranslationDTO] {
  implicit val translationWrites: Writes[Translation] = Json.writes[Translation]

  override implicit def toDto(entity: Translation): TranslationDTO =
    TranslationDTO(
      lang = entity.lang,
      name = entity.description.name,
      shortDescription = entity.description.shortDescription,
      longDescription = entity.description.longDescription,
      isDefault = entity.isDefault)
}

case class Translation(
                        id: Long,
                        lang: String,
                        description: Description,
                        isDefault: Boolean)
