package inventory.entities

import play.api.libs.json.{Json, Writes}

object Extrusion {
  implicit lazy val extrusionWrites: Writes[Extrusion] = Json.writes[Extrusion]
}

case class Extrusion(
                      id: Long,
                      mpn: String,
                      name: String,
                      templateName: String,
                      imageUrl: String,
                      profileImageUrl: String)
