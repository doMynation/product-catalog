package utils.imports

import java.sql.Timestamp
import java.time.LocalDateTime
import doobie.util.Meta

package object implicits {

  implicit class BooleanOps(b: Boolean) {
    def toInt: Int = if (b) 1 else 0
  }

  implicit val DateTimeMeta: Meta[LocalDateTime] =
    Meta[Timestamp].timap(ts => ts.toLocalDateTime)(dt => Timestamp.valueOf(dt))
}
