package inventory.entities

object Money {
  def fromDouble(amount: Double) = Money(math.ceil(amount * 100).toInt)

  def fromFloat(amount: Float) = Money(math.ceil(amount * 100).toInt)
}

case class Money(amount: Int)
