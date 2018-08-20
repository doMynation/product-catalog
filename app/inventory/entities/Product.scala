package inventory.entities

import java.time.LocalDateTime

import inventory.dtos.ProductDTO
import play.api.libs.json._
import shared.{DTOMappable, TimestampEntity}

object Product extends DTOMappable[Product, ProductDTO] {
  implicit lazy val productWrites: Writes[Product] = Json.writes[Product]

  //  implicit val productReads: Reads[Product] = (
  //    (__ \ "id").readNullable[Long] and
  //      (__ \ "sku").read[String] and
  //      (
  //        (__ \ "name").read[String] and
  //          (__ \ "short_description").read[String] and
  //          (__ \ "long_description").read[String]
  //        ) (Description.apply _) and
  //      (__ \ "retail_price").read[Double] and
  //      (__ \ "cost_price").read[Double] and
  //      (__ \ "tags").read[Seq[String]] and
  //      (__ \ "attributes").read[Seq[String]] and
  //      (__ \ "children").read[Seq[ProductChild]]
  //    ) (Product.apply _)

  override implicit def toDto(entity: Product): ProductDTO =
    ProductDTO(
      sku = entity.sku,
      categoryId = entity.categoryId,
      departmentId = entity.department.map(_.id),
      price = entity.price,
      costPrice = entity.costPrice,
      tags = entity.tags,
      metadata = entity.metadata,
      isCustom = entity.isCustom,
      isEnabled = entity.isEnabled,
      createdAt = LocalDateTime.now,
      attributes = entity.attributes.toList.sortBy(_.id).map(_.toDto),
      children = entity.children.map(_.toDto),
      rules = entity.rules.map(_.toDto),
      assemblyParts = entity.assemblyParts.map(_.toDto),
    )
}

case class Product(
                    id: Option[Long],
                    categoryId: Long,
                    sku: String,
                    descriptionId: Long,
                    description: Description,
                    price: Double,
                    costPrice: Double,
                    tags: Seq[String] = List(),
                    attributes: Set[ProductAttribute] = Set(),
                    children: Seq[ProductChild] = List(),
                    rules: Seq[ProductRule] = List(),
                    assemblyParts: Seq[ProductAssemblyPart] = List(),
                    createdAt: LocalDateTime = LocalDateTime.now,
                    updatedAt: Option[LocalDateTime] = None,
                    category: Option[ProductCategory] = None,
                    department: Option[ProductDepartment] = None,
                    metadata: Map[String, String] = Map(),
                    isCustom: Boolean,
                    isEnabled: Boolean,
                  ) extends TimestampEntity {

  def getAttribute(id: Long): Option[ProductAttribute] = attributes.find(_.attribute.id == id)

  def getAttribute(code: String): Option[ProductAttribute] = attributes.find(_.attribute.code == code)

  def replaceAttribute(a: ProductAttribute, b: ProductAttribute) = copy(attributes = attributes - a + b)
}
