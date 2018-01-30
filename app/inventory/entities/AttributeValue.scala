package inventory.entities

import java.time.LocalDateTime

import play.api.libs.json.{Json, Writes}
import shared.TimestampEntity

object AttributeValue {
  implicit val attributeValueWrites: Writes[AttributeValue] = Json.writes[AttributeValue]
}

case class AttributeValue(
                           id: Long,
                           sku: String,
                           description: Description,
                           createdAt: LocalDateTime = LocalDateTime.now,
                           updatedAt: Option[LocalDateTime] = None
                         ) extends TimestampEntity {
}
