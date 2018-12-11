package inventory.dtos

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
                           maxAllowedQuantity: Int)
