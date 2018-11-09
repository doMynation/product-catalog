package inventory.entities

import inventory.dtos.ProductChildDTO
import play.api.libs.json._
import shared.DTOMappable

object ProductChild extends DTOMappable[ProductChild, ProductChildDTO] {
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
