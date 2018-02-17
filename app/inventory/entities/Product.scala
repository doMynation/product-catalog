package inventory.entities

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import java.time.LocalDateTime
import play.api.libs.json._
import shared.TimestampEntity

object Product {
  implicit lazy val productWrites: Writes[Product] = Json.writes[Product]

  //  implicit val productReads: Reads[Product] = (
  //    (__ \ "id").readNullable[Long] and
  //      (__ \ "sku").read[String] and
  //      (
  //        (__ \ "name").read[String] and
  //          (__ \ "short_description").read[String] and
  //          (__ \ "long_description").read[String]
  //        ) (Description.apply _) and
  //      (__ \ "retail_price").read[Double] and
  //      (__ \ "cost_price").read[Double] and
  //      (__ \ "tags").read[Seq[String]] and
  //      (__ \ "attributes").read[Seq[String]] and
  //      (__ \ "children").read[Seq[ProductChild]]
  //    ) (Product.apply _)
}

case class Product(
                    id: Option[Long],
                    sku: String,
                    description: Description,
                    price: Double,
                    costPrice: Double,
                    tags: Seq[String] = List(),
                    attributes: Seq[ProductAttribute] = List(),
                    children: Seq[ProductChild] = List(),
                    rules: Seq[ProductRule] = List(),
                    createdAt: LocalDateTime = LocalDateTime.now,
                    updatedAt: Option[LocalDateTime] = None,
                    category: Option[ProductCategory] = None,
                    isCustom: Boolean,
                    metadata: Map[String, String] = Map()
                  ) extends TimestampEntity {
}
