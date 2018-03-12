package accounting.entities

import play.api.libs.json.{Json, Writes}

object InvoiceTax {
  implicit val invoiceTaxWrites: Writes[InvoiceTax] = Json.writes[InvoiceTax]
}

case class InvoiceTax(component: TaxComponent, amount: BigDecimal)
