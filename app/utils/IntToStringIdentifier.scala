package utils

trait IntToStringIdentifier {
  protected val idsMap: Map[Int, String]

  def fromId(needle: Int): Option[String] = idsMap.get(needle)

  def fromId(needle: Int, default: String): String = idsMap.getOrElse(needle, default)

  def fromString(needle: String): Option[Int] = idsMap.find(_._2 == needle).map(_._1)

  def fromString(needle: String, default: Int): Int = idsMap.find(_._2 == needle).map(_._1).getOrElse(default)
}
