package accounting.entities

import play.api.libs.json.{Json, Writes}

object InvoiceInclude {
  implicit val invoiceIncludeWrites: Writes[InvoiceInclude] = Json.writes[InvoiceInclude]
}

case class InvoiceInclude(
                           taxes: Seq[InvoiceTax] = Seq(),
                           //                           customer: Option[Customer],
                           //                           order: Option[Order],
                           //                           lineItems: Option[Seq[LineItem]]
                         ) {
}
