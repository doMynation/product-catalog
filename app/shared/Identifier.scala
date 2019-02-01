package shared

class Identifier[T](value: T) {
  override def toString: String = value.toString
}

case class ProductId(value: Long) extends Identifier(value)

case class ProductSku(value: String) extends Identifier(value)

case class InvoiceId(value: Long) extends Identifier(value)

case class InvoiceName(value: String) extends Identifier(value)

case class StoreId(value: Int) extends Identifier(value)

case class OrderId(value: Long) extends Identifier(value)

case class OrderName(value: String) extends Identifier(value)

case class QuoteId(value: Long) extends Identifier(value)
