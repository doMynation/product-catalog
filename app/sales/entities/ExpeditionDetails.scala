package sales.entities

import play.api.libs.json.{Json, Writes}
import shared.Includable

object ExpeditionDetails {
  implicit val expeditionDetailsWrites: Writes[ExpeditionDetails] = Json.writes[ExpeditionDetails]
}

case class ExpeditionDetails(
                              methodId: Long,
                              methodCode: String,
                              address: Option[Address] = None,
                              metadata: Map[String, String] = Map()
                            ) extends Includable

