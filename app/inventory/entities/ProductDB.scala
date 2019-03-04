package inventory.entities

import java.time.LocalDateTime
import utils.imports.implicits._

case class ProductDB(
                      id: Long,
                      sku: String,
                      hash: String,
                      descriptionId: Long,
                      name: String,
                      shortDescription: String,
                      longDescription: String,
                      retailPrice: Double,
                      costPrice: Double,
                      tags: String,
                      isCustom: Boolean,
                      isKit: Boolean,
                      isEnabled: Boolean,
                      mpn: Option[String],
                      imageUrl: Option[String],
                      stickerTemplateId: Option[Long],
                      extrusionTemplateId: Option[Long],
                      createdAt: LocalDateTime,
                      updatedAt: Option[LocalDateTime],
                      categoryId: Long,
                      categoryCode: String,
                      categoryName: String,
                      categoryShortDesc: String,
                      categoryLongDesc: String,
                      categoryCreatedAt: LocalDateTime,
                      categoryUpdatedAt: Option[LocalDateTime],
                      deptId: Option[Long],
                      deptCode: Option[String],
                      deptName: Option[String],
                      deptShortDesc: Option[String],
                      deptLongDesc: Option[String],
                      deptCreatedAt: Option[LocalDateTime],
                      deptUpdatedAt: Option[LocalDateTime],
                    ) {

  def toEntity: Product = {
    val metadata = Map(
      "mpn" -> mpn.getOrElse(""),
      "imageUrl" -> imageUrl.getOrElse(""),
      "isKit" -> isKit.toInt.toString,
      "stickerId" -> stickerTemplateId.map(_.toString).getOrElse(""),
      "extrusionId" -> extrusionTemplateId.map(_.toString).getOrElse(""),
    )

    val tagsSeq = tags match {
      case s if s.length > 0 => s.split(",").toList
      case _ => List[String]()
    }

    val department = deptId.map(_ => ProductDepartment(
      deptId.get,
      deptCode.get,
      Description(deptName.get, deptShortDesc.get, deptLongDesc.get),
      deptCreatedAt.get,
      deptUpdatedAt
    ))

    val category = ProductCategory(
      categoryId,
      categoryCode,
      Description(categoryName, categoryShortDesc, categoryLongDesc),
      categoryCreatedAt,
      categoryUpdatedAt,
    )

    Product(
      id = id,
      hash = hash,
      categoryId = categoryId,
      sku = sku,
      descriptionId = descriptionId,
      description = Description(name, shortDescription, longDescription),
      price = retailPrice,
      costPrice = costPrice,
      tags = tagsSeq,
      category = Some(category),
      department = department,
      createdAt = createdAt,
      updatedAt = updatedAt,
      metadata = metadata,
      isCustom = isCustom,
      isEnabled = isEnabled
    )
  }
}
