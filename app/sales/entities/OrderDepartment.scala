package sales.entities

import utils.IntToStringIdentifier

object OrderDepartment extends IntToStringIdentifier {
  val IMPORT = "import"
  val VERIFICATION = "verification"
  val CONFIRMATION = "confirmation"
  val REVIEW = "review"
  val CREATION = "creation"
  val APPROBATION = "approbation"
  val DISTRIBUTION = "distribution"
  val MEASUREMENT = "measurement"
  val DESIGN = "design"
  val PLANNING = "planning"
  val PRODUCTION = "production"
  val EXPEDITION = "expedition"
  val COMPLETE = "complete"

  override protected val idsMap: Map[Int, String] = Map(
    -5 -> IMPORT,
    -4 -> VERIFICATION,
    -3 -> CONFIRMATION,
    -2 -> REVIEW,
    -1 -> CREATION,
    0 -> APPROBATION,
    1 -> DISTRIBUTION,
    2 -> MEASUREMENT,
    3 -> DESIGN,
    4 -> PLANNING,
    5 -> PRODUCTION,
    6 -> EXPEDITION,
    7 -> COMPLETE
  )
}
