package inventory.entities

import inventory.dtos.ProductChildDTO
import play.api.libs.json._
import shared.DTOMappable

object ProductChild extends DTOMappable[ProductChild, ProductChildDTO] {
  implicit val productChildWrites: Writes[ProductChild] = Json.writes[ProductChild]

  //  implicit val productChildReads: Reads[ProductChild] = (
  //    (__ \ "product").read[Product] and
  //      (__ \ "type").read[String] and
  //      (__ \ "quantity").read[Long] and
  //      (__ \ "is_visible").read[Boolean] and
  //      (__ \ "is_compiled").read[Boolean]
  //    ) (ProductChild.apply _)

  override implicit def toDto(entity: ProductChild): ProductChildDTO =
    ProductChildDTO(
      entity.product.id.get,
      entity.childType,
      entity.quantity,
      entity.isVisible,
      entity.isCompiled,
    )
}

case class ProductChild(
                         product: Product,
                         childType: String,
                         quantity: Long,
                         isVisible: Boolean,
                         isCompiled: Boolean)
