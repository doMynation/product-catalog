package inventory.dtos

import play.api.libs.json.Reads
import play.api.libs.json._
import play.api.libs.functional.syntax._

object ProductChildDTO {
  implicit val reads: Reads[ProductChildDTO] = (
    (__ \ "productId").read[Long] and
      (__ \ "childType").read[String] and
      (__ \ "quantity").read[Long] and
      (__ \ "isVisible").read[Boolean] and
      (__ \ "isCompiled").read[Boolean]
    ) (ProductChildDTO.apply _)
}

case class ProductChildDTO(
                            productId: Long,
                            childType: String,
                            quantity: Long,
                            isVisible: Boolean,
                            isCompiled: Boolean)

