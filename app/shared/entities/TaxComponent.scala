package shared.entities

import play.api.libs.json.{Json, Writes}

object TaxComponent {
  implicit val taxComponentWrites: Writes[TaxComponent] = Json.writes[TaxComponent]
}

case class TaxComponent(id: Long, name: String, rate: BigDecimal)
