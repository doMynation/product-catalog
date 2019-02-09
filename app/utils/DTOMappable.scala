package utils

trait DTOMappable[A, B] {
  self =>

  implicit class DTOOps(entity: A) {
    def toDto: B = self.toDto(entity)
  }

  implicit def toDto(entity: A): B
}
