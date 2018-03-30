package sales.entities

import shared.entities.IntToStringIdentifier

object OrderType extends IntToStringIdentifier {
  val MODEL = "model"
  val PARTS = "parts"

  override protected val idsMap: Map[Int, String] = Map(
    1 -> MODEL,
    2 -> PARTS
  )
}
