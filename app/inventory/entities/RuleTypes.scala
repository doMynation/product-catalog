package inventory.entities

object RuleTypes {
  val NORMAL = "normal"
  val INCLUDED = "included"
  val MANDATORY = "mandatory"

  def isValid(ruleType: String): Boolean =
    ruleType == NORMAL || ruleType == INCLUDED || ruleType == MANDATORY
}
