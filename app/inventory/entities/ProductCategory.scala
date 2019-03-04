package inventory.entities

import java.time.LocalDateTime

import play.api.libs.json.{Json, Writes}
import shared.entities.TimestampEntity

import scala.collection.immutable.SortedSet

object ProductCategory {
  implicit val categoryWrites: Writes[ProductCategory] = Json.writes[ProductCategory]

  def apply2(
              id: Long,
              code: String,
              description: Description,
              createdAt: LocalDateTime = LocalDateTime.now,
              updatedAt: Option[LocalDateTime] = None,
            ): ProductCategory = new ProductCategory(id, code, description, createdAt, updatedAt)

  def tupled = (ProductCategory.apply2 _).tupled
}

case class ProductCategory(
                            id: Long,
                            code: String,
                            description: Description,
                            createdAt: LocalDateTime = LocalDateTime.now,
                            updatedAt: Option[LocalDateTime] = None,
                            parents: SortedSet[String] = SortedSet()
                          ) extends TimestampEntity
