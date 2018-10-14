package shared.entities

import shared.IntToStringIdentifier

object Lang extends IntToStringIdentifier {
  val FR = "fr"
  val EN = "en"
  val ES = "es"

  override protected val idsMap = Map(
    1 -> FR,
    2 -> EN,
    3 -> ES,
  )
}
