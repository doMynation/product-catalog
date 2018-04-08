package shared

import accounting.entities.InvoiceTaxes
import play.api.libs.json.{Json, Writes}
import sales.entities.Customer

trait Includable {}

object Includable {
  implicit val includableWrites = new Writes[Includable] {
    override def writes(o: Includable) = o match {
      case a: Customer => Json.toJson(a)
      case b: InvoiceTaxes => Json.toJson(b)
      case c: LineItems => Json.toJson(c)
    }
  }
}
