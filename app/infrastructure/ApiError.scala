package infrastructure

import play.api.libs.json.{JsValue, Json, Writes}

object ApiError {
  implicit val writes: Writes[ApiError] = Json.writes[ApiError]
}

case class ApiError(code: String, message: String) {
  def toJson(implicit ev: Writes[ApiError]): JsValue = ev.writes(this)
}
