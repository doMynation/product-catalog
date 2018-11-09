package infrastructure

import play.api.libs.json.{Json, Writes}

object ApiResponse {
  implicit def writes[T](implicit ev: Writes[T]): Writes[ApiResponse[T]] = Json.writes[ApiResponse[T]]
}

case class ApiResponse[T](data: T, meta: Map[String, String] = Map())
