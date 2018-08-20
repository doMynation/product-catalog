package inventory.entities.Admin

import inventory.entities.Translation
import play.api.libs.json.{Json, Writes}

object ProductEditData {
  implicit val productEditDataWrites: Writes[ProductEditData] = Json.writes[ProductEditData]
}

case class ProductEditData(
                            product: inventory.entities.Product,
                            translations: Seq[Translation]
                          )
