package shared

import java.time.LocalDateTime
import play.api.libs.json.{Json, Writes}

object File {
  implicit val fileWrites: Writes[File] = Json.writes[File]
}

case class File(
                 name: String,
                 description: String,
                 url: String,
                 size: Long,
                 createdAt: LocalDateTime = LocalDateTime.now)

