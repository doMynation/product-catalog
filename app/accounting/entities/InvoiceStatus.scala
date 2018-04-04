package accounting.entities

import shared.entities.IntToStringIdentifier

object InvoiceStatus extends IntToStringIdentifier {
  val CANCELLED = "cancelled"
  val NORMAL = "normal"
  val PAID = "paid"
  val IMPORT = "import"

  override protected val idsMap: Map[Int, String] = Map(
    -5 -> IMPORT,
    -1 -> CANCELLED,
    1 -> NORMAL,
    2 -> PAID
  )
}
