package accounting.entities

import java.time.LocalDateTime
import java.util.UUID

import play.api.libs.json.{Json, Writes}
import shared.entities.TimestampEntity

object Invoice {

  case class InvoiceDB(
                        id: Long,
                        uuid: String,
                        name: String,
                        storeId: Long,
                        authorId: Long,
                        subtotal: BigDecimal,
                        total: BigDecimal,
                        paidAmount: BigDecimal,
                        currencyId: Int,
                        typeId: Int,
                        statusId: Int,
                        createdAt: LocalDateTime,
                        updatedAt: Option[LocalDateTime],
                        note: String,
                        customerId: Long,
                        customerName: String,
                        orderId: Option[Long],
                        orderName: Option[String],
                      ) {
    def toEntity: Invoice = {
      val currency = if (currencyId == 1) Currency.CAD else Currency.USD
      val invoiceType = if (typeId == 1) InvoiceType.INVOICE else InvoiceType.CONTRACT
      val invoiceStatus = InvoiceStatus.fromId(statusId).getOrElse(InvoiceStatus.NORMAL)
      val metadata = Map(
        "note" -> note,
        "customerName" -> customerName,
        "orderName" -> orderName.getOrElse("")
      )

      Invoice(id, UUID.fromString(uuid), name, orderId, customerId, authorId, storeId, subtotal, total, paidAmount, currency, createdAt, updatedAt, invoiceType, invoiceStatus, metadata)
    }
  }

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
