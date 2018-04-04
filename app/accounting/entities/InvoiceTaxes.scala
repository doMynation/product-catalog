package accounting.entities

import play.api.libs.json._
import shared.Includable

object InvoiceTaxes {
  implicit val invoiceTaxesWrites: Writes[InvoiceTaxes] = new Writes[InvoiceTaxes] {
    override def writes(o: InvoiceTaxes): JsValue = JsArray(o.taxes.map { tuple =>
      JsObject(Seq(
        "component" -> Json.toJson(tuple._1),
        "amount" -> JsNumber(tuple._2)
      ))
    })
  }
}

case class InvoiceTaxes(taxes: Seq[(TaxComponent, BigDecimal)]) extends Includable
