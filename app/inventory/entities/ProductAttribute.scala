package inventory.entities

import play.api.libs.json.{Json, Writes}

object ProductAttribute {
  implicit val productAttributeWrites: Writes[ProductAttribute] = Json.writes[ProductAttribute]
}

case class ProductAttribute(
                             id: Option[Long],
                             attribute: Attribute,
                             value: String,
                             valueId: Option[Long] = None,
                             valueSku: Option[String] = None,
                             isEditable: Boolean,
                             isReference: Boolean
                           )
