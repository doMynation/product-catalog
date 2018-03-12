package accounting.entities

object InvoiceStatus {
  val CANCELLED = "cancelled"
  val NORMAL = "normal"
  val PAID = "paid"
  val IMPORT = "import"

  private val idsMap: Map[Int, String] = Map(
    -5 -> IMPORT,
    -1 -> CANCELLED,
    1 -> NORMAL,
    2 -> PAID
  )

  def fromId(needle: Int): Option[String] = idsMap.get(needle)

  def fromString(needle: String): Option[Int] = idsMap.find(_._2 == needle).map(_._1)
}

