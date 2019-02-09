package accounting.entities

import java.time.LocalDateTime
import java.util.UUID

import play.api.libs.json.{Json, Writes}
import shared.entities.TimestampEntity

object Invoice {
  implicit val invoiceWrites: Writes[Invoice] = Json.writes[Invoice]
}

case class Invoice(
                    id: Long,
                    uuid: UUID,
                    name: String,
                    orderId: Option[Long] = None,
                    customerId: Long,
                    authorId: Long,
                    storeId: Long,
                    subtotal: BigDecimal,
                    total: BigDecimal,
                    paidAmount: BigDecimal,
                    currency: String,
                    createdAt: LocalDateTime = LocalDateTime.now,
                    updatedAt: Option[LocalDateTime] = None,
                    invoiceType: String = InvoiceType.INVOICE,
                    status: String = InvoiceStatus.NORMAL,
                    metadata: Map[String, String] = Map()
                  ) extends TimestampEntity {

  def isPaid: Boolean = status == InvoiceStatus.PAID

  def isCancelled: Boolean = status == InvoiceStatus.CANCELLED

  def balance: BigDecimal = total - paidAmount
}
