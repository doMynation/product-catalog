package accounting.entities

import java.time.LocalDateTime
import play.api.libs.json.{Json, Writes}
import shared.TimestampEntity

object Invoice {
  implicit val invoiceWrites: Writes[Invoice] = Json.writes[Invoice]
}

case class Invoice(
                    val id: Option[Long],
                    sku: String,
                    createdAt: LocalDateTime = LocalDateTime.now,
                    updatedAt: Option[LocalDateTime] = None
                  ) extends TimestampEntity {
}
