package sales.entities

import java.time.LocalDateTime
import java.util.UUID

import play.api.libs.json.{Json, Writes}
import shared.entities.TimestampEntity

object Order {
  implicit val orderWrites: Writes[Order] = Json.writes[Order]
}

case class Order(
                  id: Long,
                  uuid: UUID,
                  name: String,
                  customerId: Long,
                  authorId: Long,
                  storeId: Long,
                  tag: String = "",
                  createdAt: LocalDateTime = LocalDateTime.now,
                  updatedAt: Option[LocalDateTime] = None,
                  metadata: Map[String, String] = Map(),
                  department: String = OrderDepartment.CREATION,
                  orderType: String,
                  status: String = OrderStatus.NEW
                ) extends TimestampEntity

