package accounting.entities

import play.api.libs.json.{Json, Writes}
import sales.entities.Customer

object InvoiceInclude {
  implicit val invoiceIncludeWrites: Writes[InvoiceInclude] = Json.writes[InvoiceInclude]
}

case class InvoiceInclude(
                           taxes: Option[Seq[InvoiceTax]] = None,
                           customer: Option[Customer] = None,
                           //                           order: Option[Order],
                           lineItems: Option[Seq[LineItem]] = None
                         ) {
}
