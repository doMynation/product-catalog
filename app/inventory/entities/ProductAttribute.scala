package inventory.entities

import play.api.libs.json.{Json, Writes}

object ProductAttribute {
  implicit val productAttributeWrites: Writes[ProductAttribute] = Json.writes[ProductAttribute]
}

case class ProductAttribute(
                             id: Option[Long],
                             attribute: Attribute,
                             value: String,
                             isEditable: Boolean = true,
                             isReference: Boolean = false
                           ) {

}
