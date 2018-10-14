package inventory.dtos

import play.api.libs.json.Reads
import play.api.libs.json._
import play.api.libs.functional.syntax._

object ProductAttributeDTO {
  implicit val dtoReads: Reads[ProductAttributeDTO] = (
    (__ \ "id").read[Long] and
      (__ \ "value").read[String] and
      (__ \ "valueId").readNullable[Long] and
      (__ \ "isEditable").read[Boolean]
    ) (ProductAttributeDTO.make _)

  def make(
             attributeId: Long,
             value: String,
             valueId: Option[Long] = None,
             isEditable: Boolean): ProductAttributeDTO = ProductAttributeDTO(attributeId, value, valueId, isEditable, valueId.isDefined)
}

case class ProductAttributeDTO(
                                attributeId: Long,
                                value: String,
                                valueId: Option[Long] = None,
                                isEditable: Boolean,
                                isReference: Boolean)
