package shared

import java.time.LocalDateTime

trait TimestampEntity {
  def createdAt: LocalDateTime

  def updatedAt: Option[LocalDateTime]
}
