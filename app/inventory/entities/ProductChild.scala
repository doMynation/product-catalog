package inventory.entities

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

object ProductChild {
  implicit val productChildWrites: Writes[ProductChild] = Json.writes[ProductChild]

//  implicit val productChildReads: Reads[ProductChild] = (
//    (__ \ "product").read[Product] and
//      (__ \ "type").read[String] and
//      (__ \ "quantity").read[Long] and
//      (__ \ "is_visible").read[Boolean] and
//      (__ \ "is_compiled").read[Boolean]
//    ) (ProductChild.apply _)
}

case class ProductChild(product: Product, childType: String, quantity: Long, isVisible: Boolean, isCompiled: Boolean) {

}
