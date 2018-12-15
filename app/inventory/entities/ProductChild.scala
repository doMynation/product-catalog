package inventory.entities

import inventory.dtos.ProductChildDTO
import play.api.libs.json._
import shared.DTOMappable

object ProductChild extends DTOMappable[ProductChild, ProductChildDTO] {
  val TYPE_COMPOSED = "composed" // A parent is "composed" of this child
  val TYPE_USED = "uses" // This child is "used" by a parent

  implicit val productChildWrites: Writes[ProductChild] = Json.writes[ProductChild]

  override implicit def toDto(entity: ProductChild): ProductChildDTO =
    ProductChildDTO(
      entity.product.id,
      entity.childType,
      entity.quantity,
      entity.isVisible,
      entity.isCompiled,
    )
}

case class ProductChild(
                         id: Long,
                         product: Product,
                         childType: String,
                         quantity: Long,
                         isVisible: Boolean,
                         isCompiled: Boolean)
