package sales.entities

import shared.entities.IntToStringIdentifier

object AddressType extends IntToStringIdentifier {
  val BILLING = "billing"
  val SHIPPING = "shipping"

  override protected val idsMap = Map(
    1 -> BILLING,
    2 -> SHIPPING
  )
}

