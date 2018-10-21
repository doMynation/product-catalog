package inventory.entities

import java.time.LocalDateTime

case class Store(id: Long, name: String, createdAt: LocalDateTime = LocalDateTime.now)
