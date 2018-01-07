package inventory.entities

sealed trait Discount[T] {
  def process(original: T): T
}

final case class PercentDiscount(amount: Float) extends Discount[Float] {
  if (amount < 0.0f || amount > 100.0f) throw new IllegalArgumentException("Amount has to be between 0 and 100")

  def process(original: Float): Float = original - (original * 100 / amount)
}

final case class FlatDiscount(amount: Float) extends Discount[Float] {
  def process(original: Float): Float = original - amount
}
