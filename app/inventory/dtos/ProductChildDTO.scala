package inventory.dtos

import inventory.entities.ProductChild
import inventory.validators._
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
                            isCompiled: Boolean) {

  def validate: Either[DomainError, ProductChildDTO] =
    for {
      _ <- Either.cond(childType == ProductChild.TYPE_COMPOSED || childType == ProductChild.TYPE_USED, childType, InvalidChildType)
      _ <- Either.cond(quantity > 0, quantity, InvalidQuantity)
      _ <- Either.cond(productId > 0, productId, InvalidProductId)
    } yield this
}

