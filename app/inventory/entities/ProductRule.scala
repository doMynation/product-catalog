package inventory.entities

import inventory.dtos.ProductRuleDTO
import play.api.libs.json.{Json, Writes}
import shared.DTOMappable

object ProductRule extends DTOMappable[ProductRule, ProductRuleDTO] {
  implicit val ruleWrites: Writes[ProductRule] = Json.writes[ProductRule]

  override implicit def toDto(entity: ProductRule): ProductRuleDTO =
    ProductRuleDTO(
      entity.product.id.get,
      entity.ruleType,
      entity.newPrice,
      entity.quantity,
      entity.maxAllowedQuantity
    )
}

case class ProductRule(
                        id: Long,
                        product: Product,
                        newPrice: Double,
                        ruleType: String,
                        quantity: Int,
                        maxAllowedQuantity: Int)
