package shared

import play.api.libs.json.{JsArray, JsValue, Json, Writes}
import shared.entities.LineItem

object LineItems {
  implicit val lineItemsWrites: Writes[LineItems] = new Writes[LineItems] {
    override def writes(o: LineItems): JsValue = JsArray(
      o.items.map(Json.toJson(_))
    )
  }
}

case class LineItems(items: Seq[LineItem]) extends Includable
