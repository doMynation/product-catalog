package sales.entities

import play.api.libs.json.{Json, Writes}
import shared.Includable

object Customer {
  implicit val addressWrites: Writes[Customer] = Json.writes[Customer]
}

case class Customer(
                     id: Long,
                     fullName: String,
                     emails: Seq[String] = Seq(),
                     phoneNumbers: Seq[String] = Seq(),
                     addresses: Seq[Address] = Seq(),
                   ) extends Includable
