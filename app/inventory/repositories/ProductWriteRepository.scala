package inventory.repositories

import java.sql.Connection
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.inject.Inject

import infrastructure.DatabaseExecutionContext
import inventory.dtos._
import inventory.util.DatabaseHelper
import play.api.Logger
import play.api.db.Database
import shared.dtos.TranslationDTO

import scala.concurrent.Future
import scala.util.Try
import shared.imports.implicits._

final class ProductWriteRepository @Inject()(db: Database)(implicit ec: DatabaseExecutionContext) {
  // @todo: Move this elsewhere
  private def getLangId(langCode: String): Int = langCode match {
    case "fr" => 1
    case "en" => 2
    case "es" => 3
    case _ => 1
  }

  def updateProduct(productId: Long, fields: Map[String, String]): Future[Boolean] = Future {
    db.withConnection { conn =>
      val affected = DatabaseHelper.update("inv_products",
        List("id = @productId"),
        Map("productId" -> productId.toString),
        fields ++ Map(
          "modification_date" -> LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
        )
      )(conn)

      affected > 0
    }
  }

  def bulkUpdateProduct(productIds: Seq[Long], fields: Map[String, String]): Future[Boolean] = Future {
    db.withConnection { conn =>
      val now = LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      val setClause = fields.keys.map(column => s"$column=@$column").mkString(", ")
      val inClause = productIds.map(id => s"@p_$id").mkString(",")
      val sql = s"UPDATE inv_products SET $setClause WHERE id IN ($inClause)";
      val params = fields ++
        productIds.map(id => s"p_$id" -> id.toString).toMap +
        ("modification_date" -> now)

      val affected = DatabaseHelper.executeUpdate(sql, params)(conn)

      affected > 0
    }
  }

  def deleteProduct(productId: Long): Future[Boolean] = Future {
    db.withConnection { conn =>
      val affectedRows = DatabaseHelper.executeUpdate("DELETE FROM inv_products WHERE id = @productId", Map("productId" -> productId.toString))(conn)

      affectedRows > 0
    }
  }

  def createProduct(dto: ProductDTO): Try[Long] = db.withTransaction { implicit conn =>
    // Create a description
    val descriptionIdTry = createDescription(dto.translations)

    descriptionIdTry.map { descriptionId =>
      val productData = Map(
        "category_id" -> dto.categoryId.toString,
        "description_id" -> descriptionId.toString,
        "sku" -> dto.sku,
        "retail_price" -> dto.price.toString,
        "cost_price" -> dto.costPrice.toString,
        "tags" -> dto.tags.mkString(","),
        "is_kit" -> dto.metadata.get("isKit").getOrElse("0"),
        "is_custom" -> dto.isCustom.toInt.toString,
        "status" -> dto.isEnabled.toInt.toString,
        "creation_date" -> dto.createdAt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      ) ++
        dto.departmentId.map(v => Map("department_id" -> v.toString)).getOrElse(Map()) ++
        dto.metadata.get("mpn").map(v => Map("mpn" -> v)).getOrElse(Map()) ++
        dto.metadata.get("imageUrl").map(v => Map("image_url" -> v)).getOrElse(Map()) ++
        dto.metadata.get("stickerId").filterNot(v => v == null || v == "0").map(v => Map("sticker_template_id" -> v)).getOrElse(Map()) ++
        dto.metadata.get("extrusionId").filterNot(v => v == null || v == "0").map(v => Map("extrusion_template_id" -> v)).getOrElse(Map())

      // Insert product
      val productId = DatabaseHelper.insert("inv_products", productData)(conn)

      // @todo: DB Batch inserts
      // Persist stores
      dto.storePrices.foreach(createProductStorePrice(productId, _))

      // Persist attributes
      dto.attributes.foreach(createProductAttribute(productId, _))

      // Persist children
      dto.children.foreach(createProductChild(productId, _))

      // Persist rules
      dto.rules.foreach(createProductRule(productId, _))

      // Persist assembly
      dto.assemblyParts.foreach(createProductAssemblyPart(productId, _))

      productId
    }
  }

  def createTranslation(descriptionId: Long, translation: TranslationDTO)(implicit conn: Connection): Try[Long] = {
    Try {
      DatabaseHelper.insert("translations", Map(
        "description_id" -> descriptionId.toString,
        "lang_id" -> getLangId(translation.lang).toString,
        "label" -> translation.name,
        "short_description" -> translation.shortDescription,
        "long_description" -> translation.longDescription,
        "is_default" -> translation.isDefault.toInt.toString
      ))(conn)
    }
  }

  def createDescription(translations: Seq[TranslationDTO])(implicit conn: Connection): Try[Long] = {
    // Create the description entry
    val descriptionIdTry = Try {
      DatabaseHelper.insert("descriptions", Map(
        "creation_date" -> LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")),
        "modification_date" -> LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      ))(conn)
    }

    descriptionIdTry.map { descriptionId =>
      // Create each translations
      translations.foreach(dto => createTranslation(descriptionId, dto))

      descriptionId
    }
  }

  def createProductRule(productId: Long, rule: ProductRuleDTO)(implicit conn: Connection): Try[Long] = {
    val isOriginalPrice = rule.newPrice == 0.00

    Try {
      DatabaseHelper.insert("inv_product_relations", Map(
        "product_id" -> productId.toString,
        "related_product_id" -> rule.productId.toString,
        "price" -> rule.newPrice.toString,
        "is_original_price" -> isOriginalPrice.toInt.toString,
        "type" -> rule.ruleType,
        "quantity" -> rule.quantity.toString,
        "max_quantity" -> rule.maxAllowedQuantity.toString,
      ))(conn)
    }
  }

  def createProductChild(productId: Long, child: ProductChildDTO)(implicit conn: Connection): Try[Long] = {
    Try {
      DatabaseHelper.insert("inv_product_compositions", Map(
        "product_id" -> productId.toString,
        "sub_product_id" -> child.productId.toString,
        "type" -> child.childType,
        "quantity" -> child.quantity.toString,
        "is_compiled" -> child.isCompiled.toInt.toString,
        "is_visible" -> child.isVisible.toInt.toString,
      ))(conn)
    }
  }

  def createProductAttribute(productId: Long, dto: ProductAttributeDTO)(implicit conn: Connection): Try[Long] = {
    Logger.info(s"Adding ${dto.toString}")

    Try {
      DatabaseHelper.insert("inv_product_attributes", Map(
        "product_id" -> productId.toString,
        "attribute_id" -> dto.attributeId.toString,
        "attribute_value" -> dto.valueId.getOrElse(dto.value).toString,
        "is_reference" -> dto.isReference.toInt.toString,
        "is_editable" -> dto.isEditable.toInt.toString,
      ))(conn)
    }
  }

  def updateProductAttribute(recordId: Long, dto: ProductAttributeDTO)(implicit conn: Connection): Try[Boolean] = Try {
    val affected = DatabaseHelper.update(
      "inv_product_attributes",
      List("id = @id"),
      Map("id" -> recordId.toString),
      Map(
        "attribute_value" -> dto.valueId.getOrElse(dto.value).toString,
        "is_reference" -> dto.isReference.toInt.toString,
        "is_editable" -> dto.isEditable.toInt.toString,
      ))(conn)

    affected > 0
  }

  def createProductAssemblyPart(productId: Long, part: ProductAssemblyPartDTO)(implicit conn: Connection): Try[Long] = {
    Try {
      DatabaseHelper.insert("inv_product_assemblies", Map(
        "product_id" -> productId.toString,
        "assembly_product_id" -> part.productId.toString,
        "tag" -> part.partType,
        "is_default" -> part.isDefault.toInt.toString,
      ))(conn)
    }
  }

  def createProductStorePrice(productId: Long, storePrice: ProductStorePriceDTO)(implicit conn: Connection): Try[Long] = {
    Try {
      DatabaseHelper.insert("inv_product_stores", Map(
        "product_id" -> productId.toString,
        "store_id" -> storePrice.storeId.toString,
      ) ++ storePrice.price.map(s => Map("price" -> s.toString)).getOrElse(Map())
      )(conn)
    }
  }
}
