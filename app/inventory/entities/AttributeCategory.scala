package inventory.entities

import play.api.libs.json.{Json, Writes}

object AttributeCategory {
  implicit val attributeWrites: Writes[AttributeCategory] = Json.writes[AttributeCategory]
}

case class AttributeCategory(id: Long, description: Description) {

}
