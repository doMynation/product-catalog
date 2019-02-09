package infra.responses

import play.api.libs.json.{JsValue, Json, Writes}

object ApiResponse {
  implicit def writes[A](implicit ev: Writes[A]): Writes[ApiResponse[A]] = Json.writes[ApiResponse[A]]

  def empty = ApiResponse(Json.obj())
}

case class ApiResponse[A](data: A, meta: Map[String, String] = Map()) {
  def toJson(implicit ev: Writes[ApiResponse[A]]): JsValue = ev.writes(this)
}
