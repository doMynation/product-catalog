package inventory.util

object CsvSerializer {
  def toCsv[T <: Product](o: T) = "\"" + o.productIterator.map {
    case Some(value) => value.toString.replaceAll("\"", "\"\"")
    case None => ""
    case rest => rest.toString.replaceAll("\"", "\"\"")
  }.mkString("\",\"") + "\""
}
