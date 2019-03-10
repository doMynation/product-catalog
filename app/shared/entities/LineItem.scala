package shared.entities

import play.api.libs.json.{Json, Writes}

object LineItem {
  implicit val lineItemWrites: Writes[LineItem] = Json.writes[LineItem]

  case class LineItemDB(
                         id: Long,
                         quantity: Int,
                         retailPrice: BigDecimal,
                         unitPrice: BigDecimal,
                         typeId: Int,
                         productId: Long,
                         productName: Option[String]
                       ) {
    def toEntity: LineItem = {
      LineItem(
        id,
        if (productId == 0) None else Some(productId),
        quantity,
        retailPrice,
        unitPrice,
        LineItemType.fromId(typeId).getOrElse(LineItemType.NORMAL),
        metadata = Map(
          "productName" -> productName.getOrElse("")
        )
      )
    }
  }

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
