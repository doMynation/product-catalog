package sales.entities

import shared.entities.IntToStringIdentifier

object QuoteStatus extends IntToStringIdentifier {
  val IMPORT = "import"
  val CANCELLED = "cancelled"
  val NORMAL = "normal"

  override protected val idsMap: Map[Int, String] = Map(
    -5 -> IMPORT,
    -1 -> CANCELLED,
    1 -> NORMAL,
  )
}
