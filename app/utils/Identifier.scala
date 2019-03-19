package utils

class Identifier[A](value: A) {
  override def toString: String = value.toString
}

case class StoreId(value: Int) extends Identifier(value)
