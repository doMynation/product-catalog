package inventory.dtos

case class ProductAssemblyPartDTO(
                                   productId: Long,
                                   partType: String,
                                   isDefault: Boolean)

