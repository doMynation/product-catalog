package inventory.entities

import java.time.LocalDateTime
import play.api.libs.json.{Json, Writes}
import shared.TimestampEntity
import scala.collection.immutable.SortedSet

object ProductCategory {
  implicit val categoryWrites: Writes[ProductCategory] = Json.writes[ProductCategory]
}

case class ProductCategory(
                            id: Option[Long],
                            code: String,
                            description: Description,
                            createdAt: LocalDateTime = LocalDateTime.now,
                            updatedAt: Option[LocalDateTime] = None,
                            parents: SortedSet[String] = SortedSet()
                          ) extends TimestampEntity {
}
