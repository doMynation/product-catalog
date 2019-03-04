package inventory.entities

import java.time.LocalDateTime

import play.api.libs.json.{Json, Writes}
import shared.entities.TimestampEntity

object Attribute {
  implicit val attributeWrites: Writes[Attribute] = Json.writes[Attribute]

  def apply2(
              id: Long,
              code: String,
              dataType: String,
              inputType: String,
              description: Description,
              createdAt: LocalDateTime = LocalDateTime.now,
              updatedAt: Option[LocalDateTime] = None,
            ): Attribute = new Attribute(id, code, dataType, inputType, description, createdAt, updatedAt)

  def tupled = (Attribute.apply2 _).tupled
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
