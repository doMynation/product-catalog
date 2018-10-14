package sales.entities

import shared.IntToStringIdentifier

object OrderStatus extends IntToStringIdentifier {
  val IMPORT = "import"
  val REVIEW = "review"
  val CANCELLED = "cancelled"
  val NEW = "new"
  val APPROVED = "approved"

  override protected val idsMap: Map[Int, String] = Map(
    -5 -> IMPORT,
    -2 -> REVIEW,
    -1 -> CANCELLED,
    0 -> NEW,
    1 -> APPROVED
  )
}
