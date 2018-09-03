package inventory.entities

import java.time.LocalDateTime

import play.api.libs.json.{Json, Writes}
import shared.TimestampEntity

object Attribute {
  implicit val attributeWrites: Writes[Attribute] = Json.writes[Attribute]
}

case class Attribute(
                      id: Long,
                      code: String,
                      dataType: String,
                      inputType: String,
                      description: Description,
                      createdAt: LocalDateTime = LocalDateTime.now,
                      updatedAt: Option[LocalDateTime] = None,
                      values: Seq[AttributeValue] = Seq()
                    ) extends TimestampEntity
