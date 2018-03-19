package shared.entities

trait IntToStringIdentifier {
  protected val idsMap: Map[Int, String]

  def fromId(needle: Int): Option[String] = idsMap.get(needle)

  def fromString(needle: String): Option[Int] = idsMap.find(_._2 == needle).map(_._1)
}
