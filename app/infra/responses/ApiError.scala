package infra.responses

import inventory.validators.DomainError
import play.api.libs.json.{JsValue, Json, Writes}

object ApiError {
  implicit val writes: Writes[ApiError] = Json.writes[ApiError]

  def fromDomainError(err: DomainError): ApiError = ApiError(err.code, err.errorMessage)
}

case class ApiError(code: String, message: String) {
  def toJson(implicit ev: Writes[ApiError]): JsValue = ev.writes(this)
}
