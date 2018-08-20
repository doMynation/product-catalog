package inventory.dtos

case class ProductRuleDTO(
                           productId: Long,
                           ruleType: String,
                           newPrice: Double,
                           quantity: Int,
                           maxAllowedQuantity: Int)
