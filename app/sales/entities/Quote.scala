package sales.entities

import java.time.LocalDateTime
import java.util.UUID
import play.api.libs.json.{Json, Writes}
import shared.TimestampEntity

object Quote {
  implicit val quoteWrites: Writes[Quote] = Json.writes[Quote]
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
