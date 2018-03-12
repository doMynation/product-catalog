package sales.entities

case class Customer(
                     id: Long,
                     fullName: String,
                     phoneNumbers: Seq[String],
                     email: String,
                     billingAddress: Address,
                     shippingAddress: Address
                   )
