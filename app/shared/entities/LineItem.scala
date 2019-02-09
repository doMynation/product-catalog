package shared.entities

import play.api.libs.json.{Json, Writes}

object LineItem {
  implicit val lineItemWrites: Writes[LineItem] = Json.writes[LineItem]
}

case class LineItem(
                     id: Long,
                     productId: Option[Long] = None,
                     quantity: Int,
                     retailPrice: BigDecimal,
                     unitPrice: BigDecimal,
                     lineItemType: String,
                     product: Option[inventory.entities.Product] = None,
                     attributeOverrides: Seq[(String, String)] = Seq(),
                     metadata: Map[String, String] = Map()
                   )
