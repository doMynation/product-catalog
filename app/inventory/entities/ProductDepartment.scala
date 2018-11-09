package inventory.entities

import java.time.LocalDateTime
import play.api.libs.json.{Json, Writes}
import shared.TimestampEntity

object ProductDepartment {
  implicit val departmentWrites: Writes[ProductDepartment] = Json.writes[ProductDepartment]
}

case class ProductDepartment(
                              id: Long,
                              code: String,
                              description: Description,
                              createdAt: LocalDateTime = LocalDateTime.now,
                              updatedAt: Option[LocalDateTime] = None
                            ) extends TimestampEntity
