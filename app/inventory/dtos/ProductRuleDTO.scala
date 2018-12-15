package inventory.dtos

import inventory.entities.RuleTypes
import inventory.validators._
import play.api.libs.json.Reads
import play.api.libs.json._
import play.api.libs.functional.syntax._

object ProductRuleDTO {
  implicit val reads: Reads[ProductRuleDTO] = (
    (__ \ "productId").read[Long] and
      (__ \ "ruleType").read[String] and
      (__ \ "newPrice").read[Double] and
      (__ \ "quantity").read[Int] and
      (__ \ "maxAllowedQuantity").readWithDefault[Int](1)
    ) (ProductRuleDTO.apply _)
}

case class ProductRuleDTO(
                           productId: Long,
                           ruleType: String,
                           newPrice: Double,
                           quantity: Int,
                           maxAllowedQuantity: Int) {

  def validate: Either[DomainError, ProductRuleDTO] =
    for {
      _ <- Either.cond(RuleTypes.isValid(ruleType), ruleType, InvalidSalesRuleType)
      _ <- Either.cond(quantity > 0, quantity, InvalidQuantity)
      _ <- Either.cond(newPrice >= 0, quantity, InvalidPrice)
      _ <- Either.cond(productId > 0, productId, InvalidProductId)
    } yield this
}
