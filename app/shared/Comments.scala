package shared

import play.api.libs.json.{JsArray, JsValue, Json, Writes}

object Comments {
  implicit val commentsWrites: Writes[Comments] = new Writes[Comments] {
    override def writes(o: Comments): JsValue = JsArray(
      o.items.map(Json.toJson(_))
    )
  }
}

case class Comments(items: Seq[Comment]) extends Includable
