package inventory.dtos

import play.api.libs.json._
import play.api.libs.json.Reads
import play.api.libs.functional.syntax._

object AttributeIdValuePair {
  implicit val attrIdValPairReads: Reads[AttributeIdValuePair] = (
    (__ \ "id").read[Long] and
      (__ \ "value").read[String] and
      (__ \ "isEditable").readWithDefault[Boolean](false)
    ) (AttributeIdValuePair.apply _)
}

case class AttributeIdValuePair(id: Long, value: String, isEditable: Boolean)

