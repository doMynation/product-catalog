package inventory.entities

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

object Description {
  implicit val descriptionWrites = Json.writes[Description]

  implicit val jsonReads: Reads[Description] = (
    (__ \ "name").read[String] and
      (__ \ "short_description").read[String] and
      (__ \ "long_description").read[String]
    ) (Description.apply _)
}

case class Description(
                        name: String,
                        shortDescription: String,
                        longDescription: String) {

}
