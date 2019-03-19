package sales.entities

import java.time.LocalDateTime
import java.util.UUID
import accounting.entities.{Currency}
import play.api.libs.json.{Json, Writes}
import shared.entities.TimestampEntity

object Quote {
  implicit val quoteWrites: Writes[Quote] = Json.writes[Quote]

  case class QuoteDB(
                      id: Long,
                      uuid: String,
                      name: String,
                      subtotal: BigDecimal,
                      total: BigDecimal,
                      currencyId: Int,
                      note: String,
                      statusId: Int,
                      authorId: Long,
                      storeId: Long,
                      customerId: Long,
                      customerName: String,
                      createdAt: LocalDateTime,
                      updatedAt: Option[LocalDateTime],
                    ) {
    def toEntity: Quote = {
      val currency = if (currencyId == 1) Currency.CAD else Currency.USD
      val quoteStatus = QuoteStatus.fromId(statusId).getOrElse(QuoteStatus.NORMAL)
      val metadata = Map(
        "note" -> note,
        "customerName" -> customerName,
      )

      Quote(
        id = id,
        uuid = UUID.fromString(uuid),
        name = name,
        authorId = authorId,
        storeId = storeId,
        customerId = customerId,
        subtotal = subtotal,
        total = total,
        currency = currency,
        createdAt = createdAt,
        updatedAt = updatedAt,
        status = quoteStatus,
        metadata = metadata
      )
    }
  }

}

case class Quote(
                  id: Long,
                  uuid: UUID,
                  name: String,
                  customerId: Long,
                  authorId: Long,
                  storeId: Long,
                  subtotal: BigDecimal,
                  total: BigDecimal,
                  currency: String,
                  createdAt: LocalDateTime = LocalDateTime.now,
                  updatedAt: Option[LocalDateTime] = None,
                  status: String = QuoteStatus.NORMAL,
                  metadata: Map[String, String] = Map()
                ) extends TimestampEntity {

  def isCancelled: Boolean = status == QuoteStatus.CANCELLED
}
