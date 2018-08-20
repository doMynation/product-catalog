package inventory.dtos

import java.time.LocalDateTime
import shared.dtos.{TranslationDTO}

case class ProductDTO(
                       sku: String,
                       categoryId: Long,
                       departmentId: Option[Long] = None,
                       translations: Seq[TranslationDTO] = Seq(),
                       price: Double = 0,
                       costPrice: Double = 0,
                       tags: Seq[String] = List(),
                       storePrices: Seq[ProductStorePriceDTO] = List(),
                       attributes: Seq[ProductAttributeDTO] = List(),
                       children: Seq[ProductChildDTO] = List(),
                       rules: Seq[ProductRuleDTO] = List(),
                       assemblyParts: Seq[ProductAssemblyPartDTO] = List(),
                       createdAt: LocalDateTime = LocalDateTime.now,
                       updatedAt: Option[LocalDateTime] = None,
                       metadata: Map[String, String] = Map(),
                       isCustom: Boolean = false,
                       isEnabled: Boolean = true)
