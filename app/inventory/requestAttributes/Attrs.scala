package inventory.requestAttributes

import inventory.entities.Store
import play.api.libs.typedmap.TypedKey

object Attrs {
  val Username: TypedKey[String] = TypedKey.apply[String]("user")
  val Store: TypedKey[Store] = TypedKey.apply[Store]("store")
}
