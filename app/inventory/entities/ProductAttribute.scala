package inventory.entities

import inventory.dtos.ProductAttributeDTO
import play.api.libs.json.{Json, Writes}
import shared.DTOMappable

object ProductAttribute extends DTOMappable[ProductAttribute, ProductAttributeDTO] {
  implicit val productAttributeWrites: Writes[ProductAttribute] = Json.writes[ProductAttribute]

  override implicit def toDto(entity: ProductAttribute): ProductAttributeDTO =
    ProductAttributeDTO(
      entity.attribute.id,
      entity.value,
      entity.valueId,
      entity.isEditable,
      entity.isReference
    )
}

case class ProductAttribute(
                             id: Long,
                             attribute: Attribute,
                             value: String,
                             valueId: Option[Long] = None,
                             valueSku: Option[String] = None,
                             isEditable: Boolean,
                             isReference: Boolean)
