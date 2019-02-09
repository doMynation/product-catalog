package utils

class Identifier[A](value: A) {
  override def toString: String = value.toString
}

case class InvoiceId(value: Long) extends Identifier(value)

case class StoreId(value: Int) extends Identifier(value)

case class OrderId(value: Long) extends Identifier(value)

case class QuoteId(value: Long) extends Identifier(value)
