package accounting.entities

import utils.IntToStringIdentifier

object InvoiceStatus extends IntToStringIdentifier {
  val IMPORT = "import"
  val CANCELLED = "cancelled"
  val NORMAL = "normal"
  val PAID = "paid"

  override protected val idsMap: Map[Int, String] = Map(
    -5 -> IMPORT,
    -1 -> CANCELLED,
    1 -> NORMAL,
    2 -> PAID
  )
}

