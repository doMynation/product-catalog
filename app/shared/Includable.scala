package shared

import play.api.libs.json.{Json, Writes}
import sales.entities.{Customer, ExpeditionDetails}

trait Includable {}

object Includable {
  implicit val includableWrites = new Writes[Includable] {
    override def writes(o: Includable) = o match {
      case a: Customer => Json.toJson(a)
      case b: ApplicableTaxes => Json.toJson(b)
      case c: LineItems => Json.toJson(c)
      case d: ExpeditionDetails => Json.toJson(d)
      case e: FilesCollection => Json.toJson(e)
      case f: Comments => Json.toJson(f)
    }
  }
}
