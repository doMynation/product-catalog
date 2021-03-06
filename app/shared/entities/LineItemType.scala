package shared.entities

import utils.IntToStringIdentifier

object LineItemType extends IntToStringIdentifier {
  val NORMAL = "normal"
  val INCLUDED = "included"

  override protected val idsMap: Map[Int, String] = Map(
    1 -> NORMAL,
    2 -> INCLUDED
  )
}

