package shared

import java.time.LocalDateTime
import play.api.libs.json.{Json, Writes}

object Comment {
  implicit val commentWrites: Writes[Comment] = Json.writes[Comment]
}

case class Comment(
                    id: Long,
                    authorId: Long,
                    authorName: String,
                    authorStoreId: Long,
                    authorStoreName: String,
                    customerId: Long,
                    customerName: String,
                    content: String,
                    createdAt: LocalDateTime
                  )
