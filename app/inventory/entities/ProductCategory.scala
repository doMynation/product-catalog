package inventory.entities

import java.time.LocalDateTime
import play.api.libs.json.{Json, Writes}
import shared.TimestampEntity

object ProductCategory {
  implicit val categoryWrites: Writes[ProductCategory] = Json.writes[ProductCategory]
}

case class ProductCategory(
                            id: Option[Long],
                            description: Description,
                            createdAt: LocalDateTime = LocalDateTime.now,
                            updatedAt: Option[LocalDateTime] = None
                          ) extends TimestampEntity {
}
