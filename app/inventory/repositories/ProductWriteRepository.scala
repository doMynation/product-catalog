package inventory.repositories

import java.sql.Connection
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import javax.inject.Inject

import inventory.dtos._
import inventory.util.DB
import play.api.db.Database
import shared.Types.Product
import shared.dtos.TranslationDTO
import shared.entities.Lang

import scala.util.Try
import utils.imports.implicits._

final class ProductWriteRepository @Inject()(db: Database) {
  def updateProduct(product: Product, dto: ProductDTO): Unit = {
    val newHash = UUID.randomUUID().toString
    val baseFields: Map[String, String] = Map(
      "hash" -> newHash,
      "sku" -> dto.sku,
      "category_id" -> dto.categoryId.toString,
      "department_id" -> dto.departmentId.map(_.toString).getOrElse(null),
      "retail_price" -> dto.price.toString,
      "cost_price" -> dto.costPrice.toString,
      "tags" -> dto.tags.mkString(","),
      "is_custom" -> dto.isCustom.toInt.toString,
      "status" -> dto.isEnabled.toInt.toString,
      "is_kit" -> dto.metadata.getOrElse("isKit", "0"),
      "image_url" -> dto.metadata.getOrElse("imageUrl", ""),
      "mpn" -> dto.metadata.getOrElse("mpn", ""),
      "extrusion_template_id" -> dto.metadata.get("extrusionId").filterNot(_.isEmpty).getOrElse(null),
      "sticker_template_id" -> dto.metadata.get("stickerId").filterNot(_.isEmpty).getOrElse(null)
    ) ++ dto.updatedAt.map(v => Map("modification_date" -> v.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")))).getOrElse(Map())

    db.withTransaction { implicit conn =>
      // Update product
      updateProductFields(product.id, baseFields)

      // Handle translations
      deleteTranslations(product.descriptionId)
      dto.translations.foreach(createTranslation(product.descriptionId, _))

      // Handle attributes
      deleteProductAttributes(product.id)
      dto.attributes.foreach(createProductAttribute(product.id, _))

      // Handle children
      deleteProductChildren(product.id)
      dto.children.foreach(createProductChild(product.id, _))

      // Handle rules
      deleteProductRules(product.id)
      dto.rules.foreach(createProductRule(product.id, _))
    }
  }

  def updateProductFields(productId: Long, fields: Map[String, String])(implicit connection: Connection = null): Boolean = {
    val task = (connection: Connection) => {
      val affected = DB.update("inv_products",
        List("id = @productId"),
        Map("productId" -> productId.toString),
        fields ++ Map(
          "modification_date" -> LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
        )
      )(connection)

      affected > 0
    }

    if (connection == null) db.withConnection(task)
    else task(connection)
  }

  def bulkUpdateProduct(productIds: Seq[Long], fields: Map[String, String]): Boolean = {
    val now = LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    val setClause = fields.keys.map(column => s"$column=@$column").mkString(", ")
    val inClause = productIds.map(id => s"@p_$id").mkString(",")
    val sql = s"UPDATE inv_products SET $setClause WHERE id IN ($inClause)"
    val params = fields ++
      productIds.map(id => s"p_$id" -> id.toString).toMap +
      ("modification_date" -> now)

    val query = DB.executeUpdate(sql, params) _
    val affected = db.withConnection(query)

    affected > 0
  }

  def deleteProduct(productId: Long): Boolean = db.withConnection { conn =>
    val affectedRows = DB.executeUpdate("DELETE FROM inv_products WHERE id = @productId", Map("productId" -> productId.toString))(conn)

    affectedRows > 0
  }

  def deleteTranslations(descriptionId: Long)(implicit connection: Connection): Boolean = {
    val affectedRows = DB.executeUpdate(
      "DELETE FROM translations WHERE description_id = @descriptionId",
      Map("descriptionId" -> descriptionId.toString)
    )(connection)

    affectedRows > 0
  }

  def deleteProductAttributes(productId: Long)(implicit connection: Connection): Boolean = {
    val affectedRows = DB.executeUpdate(
      "DELETE FROM inv_product_attributes WHERE product_id = @productId",
      Map("productId" -> productId.toString)
    )(connection)

    affectedRows > 0
  }

  def deleteProductChildren(productId: Long)(implicit connection: Connection): Boolean = {
    val affectedRows = DB.executeUpdate(
      "DELETE FROM inv_product_compositions WHERE product_id = @productId",
      Map("productId" -> productId.toString)
    )(connection)

    affectedRows > 0
  }

  def deleteProductRules(productId: Long)(implicit connection: Connection): Boolean = {
    val affectedRows = DB.executeUpdate(
      "DELETE FROM inv_product_relations WHERE product_id = @productId",
      Map("productId" -> productId.toString)
    )(connection)

    affectedRows > 0
  }

  def createProduct(dto: ProductDTO): Try[Long] = db.withTransaction { implicit conn =>
    // Create a description
    val descriptionIdTry = createDescription(dto.translations)

    descriptionIdTry.map { descriptionId =>
      val productData = Map(
        "hash" -> UUID.randomUUID().toString,
        "category_id" -> dto.categoryId.toString,
        "description_id" -> descriptionId.toString,
        "sku" -> dto.sku,
        "retail_price" -> dto.price.toString,
        "cost_price" -> dto.costPrice.toString,
        "tags" -> dto.tags.mkString(","),
        "is_kit" -> dto.metadata.getOrElse("isKit", "0"),
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
      val productId = DB.insert("inv_products", productData)(conn)

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
      DB.insert("translations", Map(
        "description_id" -> descriptionId.toString,
        "lang_id" -> Lang.fromString(translation.lang, 1).toString,
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
      DB.insert("descriptions", Map(
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
      DB.insert("inv_product_relations", Map(
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
      DB.insert("inv_product_compositions", Map(
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
    Try {
      DB.insert("inv_product_attributes", Map(
        "product_id" -> productId.toString,
        "attribute_id" -> dto.attributeId.toString,
        "attribute_value" -> dto.valueId.getOrElse(dto.value).toString,
        "is_reference" -> dto.isReference.toInt.toString,
        "is_editable" -> dto.isEditable.toInt.toString,
      ))(conn)
    }
  }

  def updateProductAttribute(recordId: Long, dto: ProductAttributeDTO)(implicit conn: Connection): Boolean = {
    val affected = DB.update(
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
      DB.insert("inv_product_assemblies", Map(
        "product_id" -> productId.toString,
        "assembly_product_id" -> part.productId.toString,
        "tag" -> part.partType,
        "is_default" -> part.isDefault.toInt.toString,
      ))(conn)
    }
  }

  def createProductStorePrice(productId: Long, storePrice: ProductStorePriceDTO)(implicit conn: Connection): Try[Long] = {
    Try {
      DB.insert("inv_product_stores", Map(
        "product_id" -> productId.toString,
        "store_id" -> storePrice.storeId.toString,
      ) ++ storePrice.price.map(s => Map("price" -> s.toString)).getOrElse(Map())
      )(conn)
    }
  }

  def createDepartment(dto: ProductDepartmentDTO): Long = {
    val task = (conn: Connection) => {
      val dit = createDescription(dto.translations)(conn)
      val nextDisplayOrder = DB.fetchColumn[Int]("SELECT MAX(display_order) FROM inv_departments")(conn)

      DB.insert("inv_departments", Map(
        "description_id" -> dit.get.toString, // @todo unsafe, fix this
        "code" -> dto.code,
        "display_order" -> nextDisplayOrder.get.toString, // @todo unsafe, fix this
        "creation_date" -> LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss"))
      ))(conn)
    }

    db.withConnection(task)
  }
}
