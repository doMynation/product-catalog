package inventory.forms

import java.time.LocalDateTime

import inventory.dtos._
import inventory.repositories.{MiscRepository, ProductRepository}
import inventory.validators.{DomainError, EditProductValidator}
import shared.dtos.TranslationDTO
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import shared.DTOMappable

object EditProductForm extends DTOMappable[EditProductForm, ProductDTO] {
  val nullToStringMapReader: Reads[Map[String, String]] = new Reads[Map[String, String]] {
    override def reads(json: JsValue): JsResult[Map[String, String]] = JsSuccess {
      json.as[JsObject].value.map {
        case (k, v) => (k, v match {
          case s: JsString => s.as[String]
          case _ => ""
        })
      }.toMap
    }
  }

  val emptyStringToNullReader: Reads[Option[Long]] = new Reads[Option[Long]] {
    override def reads(json: JsValue): JsResult[Option[Long]] = JsSuccess {
      json match {
        case s: JsNumber => Some(s.as[Long])
        case _ => Option.empty[Long]
      }
    }
  }

  implicit val reads: Reads[EditProductForm] = (
    (__ \ "hash").read[String] and
      (__ \ "sku").read[String] and
      (__ \ "categoryId").read[Long] and
      (__ \ "departmentId").read[Option[Long]](emptyStringToNullReader) and
      (__ \ "translations").read[List[TranslationDTO]] and
      (__ \ "price").read[Double] and
      (__ \ "costPrice").read[Double] and
      (__ \ "tags").read[List[String]] and
      Reads.pure(List.empty[ProductStorePriceDTO]) and // @todo
      (__ \ "attributes").read[List[AttributeIdValuePair]] and
      (__ \ "children").read[List[ProductChildDTO]] and
      (__ \ "salesRules").read[List[ProductRuleDTO]] and
      Reads.pure(List.empty[ProductAssemblyPartDTO]) and // @todo
      Reads.pure(LocalDateTime.now) and
      Reads.pure(Some(LocalDateTime.now)) and
      (__ \ "metadata").read[Map[String, String]](nullToStringMapReader) and
      (__ \ "isCustom").read[Boolean] and
      (__ \ "isEnabled").read[Boolean]
    ) (EditProductForm.apply _)

  override implicit def toDto(entity: EditProductForm): ProductDTO =
    ProductDTO(
      sku = entity.sku,
      categoryId = entity.categoryId,
      departmentId = entity.departmentId,
      price = entity.price,
      costPrice = entity.costPrice,
      tags = entity.tags,
      isCustom = entity.isCustom,
      isEnabled = entity.isEnabled,
      metadata = entity.metadata,
      updatedAt = Some(LocalDateTime.now),
      translations = entity.translations,
      children = entity.children,
      rules = entity.rules
    )
}

case class EditProductForm(
                            hash: String,
                            sku: String,
                            categoryId: Long,
                            departmentId: Option[Long] = None,
                            translations: Seq[TranslationDTO] = Seq(),
                            price: Double = 0,
                            costPrice: Double = 0,
                            tags: Seq[String] = List(),
                            storePrices: Seq[ProductStorePriceDTO] = List(),
                            attributes: Seq[AttributeIdValuePair] = List(),
                            children: Seq[ProductChildDTO] = List(),
                            rules: Seq[ProductRuleDTO] = List(),
                            assemblyParts: Seq[ProductAssemblyPartDTO] = List(),
                            createdAt: LocalDateTime = LocalDateTime.now,
                            updatedAt: Option[LocalDateTime] = None,
                            metadata: Map[String, String] = Map(),
                            isCustom: Boolean = false,
                            isEnabled: Boolean = true) {

  def validate(productRepository: ProductRepository, miscRepository: MiscRepository): Either[DomainError, ProductDTO] =
    EditProductValidator.validate(this, productRepository, miscRepository)
}

