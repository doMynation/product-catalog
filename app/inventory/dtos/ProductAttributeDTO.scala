package inventory.dtos

case class ProductAttributeDTO(
                                attributeId: Long,
                                value: String,
                                valueId: Option[Long] = None,
                                isEditable: Boolean,
                                isReference: Boolean)
