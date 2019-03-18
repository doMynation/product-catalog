package sales.entities

import java.time.LocalDateTime
import java.util.UUID

import accounting.entities.InvoiceStatus
import play.api.libs.json.{Json, Writes}
import shared.entities.TimestampEntity

object Order {
  implicit val orderWrites: Writes[Order] = Json.writes[Order]

  case class OrderDB(
                      id: Long,
                      uuid: String,
                      name: String,
                      typeId: Int,
                      departmentId: Int,
                      statusId: Int,
                      storeId: Long,
                      authorId: Long,
                      authorName: String,
                      customerId: Long,
                      customerName: String,
                      notes: String,
                      tag: String,
                      createdAt: LocalDateTime,
                      updatedAt: Option[LocalDateTime],
                      cancelReason: Option[String],
                      expeditedAt: Option[String],
                      invoiceId: Option[Long],
                      invoiceStatusId: Option[Int],
                      modelSku: Option[String],
                    ) {
    def toEntity: Order = {

      val invoiceStatus = for {
        statusId <- invoiceStatusId
        statusString <- InvoiceStatus.fromId(statusId.toInt)
      } yield "invoiceStatus" -> statusString

      val metadata = Map(
        "note" -> notes,
        "customerName" -> customerName,
        "authorName" -> authorName,
        "cancelReason" -> cancelReason.getOrElse(""),
        "expeditedAt" -> expeditedAt.getOrElse(""),
      ) ++
        modelSku.map("modelSku" -> _) ++
        invoiceId.map("invoiceId" -> _.toString) ++
        invoiceStatus

      Order(
        id = id,
        uuid = UUID.fromString(uuid),
        name = name,
        customerId = customerId,
        authorId = authorId,
        storeId = storeId,
        tag = tag,
        createdAt = createdAt,
        updatedAt = updatedAt,
        metadata = metadata,
        department = OrderDepartment.fromId(departmentId).getOrElse(OrderDepartment.CREATION),
        orderType = OrderType.fromId(typeId).getOrElse(OrderType.PARTS),
        status = OrderStatus.fromId(statusId).getOrElse(OrderStatus.NEW)
      )
    }
  }

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

