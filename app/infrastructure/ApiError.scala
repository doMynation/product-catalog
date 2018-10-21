package infrastructure

import play.api.libs.json.{Json, Writes}

object ApiError {
  val INVALID_CHECKSUM = "INVALID_CHECKSUM"

  implicit val writes: Writes[ApiError] = Json.writes[ApiError]
}

case class ApiError(code: String, message: String)
