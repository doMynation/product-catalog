package inventory.entities

import java.time.LocalDateTime

import play.api.libs.json.{Json, Writes}
import shared.TimestampEntity

object Attribute {
  implicit val attributeWrites: Writes[Attribute] = Json.writes[Attribute]
}

case class Attribute(
                      id: Long,
                      description: Description,
                      createdAt: LocalDateTime = LocalDateTime.now,
                      updatedAt: Option[LocalDateTime] = None
                    ) extends TimestampEntity {
}
