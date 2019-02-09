package utils.imports

package object implicits {

  implicit class BooleanOps(b: Boolean) {
    def toInt: Int = if (b) 1 else 0
  }

}
