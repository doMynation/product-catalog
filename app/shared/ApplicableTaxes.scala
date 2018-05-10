package shared

import play.api.libs.json._

object ApplicableTaxes {
  implicit val invoiceTaxesWrites: Writes[ApplicableTaxes] = new Writes[ApplicableTaxes] {
    override def writes(o: ApplicableTaxes): JsValue = JsArray(o.taxes.map { tuple =>
      JsObject(Seq(
        "component" -> Json.toJson(tuple._1),
        "amount" -> JsNumber(tuple._2)
      ))
    })
  }
}

case class ApplicableTaxes(taxes: Seq[(TaxComponent, BigDecimal)]) extends Includable
