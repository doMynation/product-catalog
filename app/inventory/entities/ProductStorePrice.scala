package inventory.entities

import inventory.dtos.ProductStorePriceDTO
import shared.DTOMappable

object ProductStorePrice extends DTOMappable[ProductStorePrice, ProductStorePriceDTO] {
  override implicit def toDto(entity: ProductStorePrice): ProductStorePriceDTO = ProductStorePriceDTO(
    entity.storeId,
    entity.price
  )
}

case class ProductStorePrice(storeId: Long, price: Option[BigDecimal])
