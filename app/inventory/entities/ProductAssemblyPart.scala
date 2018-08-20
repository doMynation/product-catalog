package inventory.entities

import inventory.dtos.ProductAssemblyPartDTO
import play.api.libs.json.{Json, Writes}
import shared.DTOMappable

object ProductAssemblyPart extends DTOMappable[ProductAssemblyPart, ProductAssemblyPartDTO] {
  implicit val productChildWrites: Writes[ProductAssemblyPart] = Json.writes[ProductAssemblyPart]

  override implicit def toDto(entity: ProductAssemblyPart): ProductAssemblyPartDTO =
    ProductAssemblyPartDTO(
      entity.part.id.get,
      entity.partType,
      entity.isDefault,
    )
}

case class ProductAssemblyPart(
                                part: Product,
                                partType: String,
                                isDefault: Boolean)
