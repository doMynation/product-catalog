package inventory.entities

import play.api.libs.json.{Json, Writes}

object ProductRule {
  implicit val ruleWrites: Writes[ProductRule] = Json.writes[ProductRule]
}

case class ProductRule(
                        id: Long,
                        product: Product,
                        newPrice: Double,
                        ruleType: String,
                        quantity: Int,
                        maxAllowedQuantity: Int) {
}
