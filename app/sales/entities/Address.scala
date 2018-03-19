package sales.entities

import play.api.libs.json.{Json, Writes}

object Address {
  implicit val addressWrites: Writes[Address] = Json.writes[Address]
}

case class Address(
                    addressType: String,
                    addressLine1: String,
                    addressLine2: String,
                    addressLine3: String = "",
                    city: String,
                    state: String,
                    country: String,
                    zipCode: String
                  )
