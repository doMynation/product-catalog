package infra.responses

import play.api.libs.json.{JsValue, Json, Writes}

object ApiResponse {
  implicit def writes[T](implicit ev: Writes[T]): Writes[ApiResponse[T]] = Json.writes[ApiResponse[T]]

  def empty = ApiResponse(Json.obj())
}

case class ApiResponse[T](data: T, meta: Map[String, String] = Map()) {
  def toJson(implicit ev: Writes[ApiResponse[T]]): JsValue = ev.writes(this)
}
