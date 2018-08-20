package inventory.dtos

case class ProductChildDTO(
                            productId: Long,
                            childType: String,
                            quantity: Long,
                            isVisible: Boolean,
                            isCompiled: Boolean)

