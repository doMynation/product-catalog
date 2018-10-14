package inventory.dtos

import java.time.LocalDateTime
import shared.dtos.TranslationDTO
import play.api.libs.json.Reads
import play.api.libs.json._
import play.api.libs.functional.syntax._

object ProductDTO {
  implicit val dtoReads: Reads[ProductDTO] = (
    (__ \ "sku").read[String] and
      (__ \ "categoryId").read[Long] and
      (__ \ "departmentId").readNullable[Long] and
      (__ \ "translations").read[List[TranslationDTO]] and
      (__ \ "price").read[Double] and
      (__ \ "costPrice").read[Double] and
      (__ \ "tags").read[List[String]] and
      Reads.pure(List.empty[ProductStorePriceDTO]) and
      (__ \ "attributes").read[List[ProductAttributeDTO]] and
      Reads.pure(List.empty[ProductChildDTO]) and
      Reads.pure(List.empty[ProductRuleDTO]) and
      Reads.pure(List.empty[ProductAssemblyPartDTO]) and
      Reads.pure(LocalDateTime.now) and
      Reads.pure(Some(LocalDateTime.now)) and
      (__ \ "metadata").read[Map[String, String]] and
      (__ \ "isCustom").read[Boolean] and
      (__ \ "isEnabled").read[Boolean]
    ) (ProductDTO.apply _)
}

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
