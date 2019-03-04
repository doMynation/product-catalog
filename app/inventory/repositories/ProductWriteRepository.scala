package inventory.repositories

import java.sql.Connection
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import cats.implicits._
import cats.effect.{ContextShift, IO}
import doobie._
import doobie.implicits._
import javax.inject.Inject
import inventory.dtos._
import inventory.util.DB
import play.api.db.Database
import shared.Types.{Product, Tx}
import shared.dtos.TranslationDTO
import shared.entities.Lang
import utils.imports.implicits._

final class ProductWriteRepository @Inject()(db2: Tx, db: Database)(implicit cs: ContextShift[IO]) {

  def updateProduct(product: Product, dto: ProductDTO): IO[Unit] =
    db2.use(Queries.updateProduct(product, dto).transact(_))

  /**
    * @todo: Refactoring this using Doobie
    * @param productId
    * @param fields
    * @return
    */
  def updateProductFields(productId: Long, fields: Map[String, String]): IO[Boolean] = {
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

    IO {
      db.withConnection(task)
    }
  }

  /**
    * @todo: Refactor this using Doobie via Update[Type](sql).updateMany(values)
    * @param productIds
    * @param fields
    * @return
    */
  def bulkUpdateProduct(productIds: Seq[Long], fields: Map[String, String]): IO[Int] = {
    val now = LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    val setClause = fields.keys.map(column => s"$column=@$column").mkString(", ")
    val inClause = productIds.map(id => s"@p_$id").mkString(",")
    val sql = s"UPDATE inv_products SET $setClause WHERE id IN ($inClause)"
    val params = fields ++
      productIds.map(id => s"p_$id" -> id.toString).toMap +
      ("modification_date" -> now)

    val query = DB.executeUpdate(sql, params) _

    IO {
      db.withConnection(query)
    }
  }

  def deleteProduct(productId: Long): IO[Boolean] =
    db2.use(Queries.deleteProduct(productId).transact(_))

  def deleteTranslations(descriptionId: Long)(implicit connection: Connection): IO[Int] =
    db2.use(Queries.deleteTranslations(descriptionId).transact(_))

  def createProduct(dto: ProductDTO): IO[Long] =
    db2.use(Queries.createProduct(dto).transact(_))

  def createDescription(translations: List[TranslationDTO])(implicit conn: Connection): IO[Long] =
    db2.use(Queries.createDescription(translations).transact(_))

  def createProductAttribute(productId: Long, dto: ProductAttributeDTO): IO[Long] =
    db2.use(Queries.createProductAttribute(productId, dto).transact(_))

  def updateProductAttribute(recordId: Long, dto: ProductAttributeDTO): IO[Boolean] =
    db2.use(Queries.updateProductAttribute(recordId, dto).transact(_))

  def createProductStorePrice(productId: Long, storePrice: ProductStorePriceDTO)(implicit conn: Connection): IO[Long] =
    db2.use(Queries.createProductStorePrice(productId, storePrice).transact(_))

  def createDepartment(dto: ProductDepartmentDTO): IO[Long] =
    db2.use(Queries.createDepartment(dto).transact(_))

  /**
    * Defines the queries without executing them. Useful for composition and sequencing database operations
    * using a single database connection.
    */
  private object Queries {
    def createDepartment(dto: ProductDepartmentDTO): ConnectionIO[Long] = {
      val insertDepartment = (descriptionId: Long, displayOrder: Int) =>
        sql"""INSERT INTO inv_departments
             |(description_id, code, display_order, creation_date) VALUES
             |(${descriptionId}, ${dto.code}, $displayOrder, ${LocalDateTime.now})
             |""".stripMargin.update.withUniqueGeneratedKeys[Long]("id")

      for {
        displayOrder <- sql"SELECT MAX(display_order) FROM inv_departments".query[Int].unique
        descriptionId <- createDescription(dto.translations)
        deptId <- insertDepartment(descriptionId, displayOrder)
      } yield deptId
    }

    def createProduct(dto: ProductDTO): ConnectionIO[Long] = {
      val insertProduct: Long => ConnectionIO[Long] = (descriptionId: Long) =>
        sql"""INSERT INTO inv_products
             |(sku, hash, description_id, category_id, department_id, mpn, retail_price, cost_price,
             |image_url, extrusion_template_id, sticker_template_id, tags,
             |is_kit, is_custom, status,
             |creation_date, modification_date) VALUES
             |(${dto.sku}, ${UUID.randomUUID.toString}, $descriptionId, ${dto.categoryId}, ${dto.departmentId}, ${dto.metadata.get("mpn")}, ${dto.price}, ${dto.costPrice},
             |${dto.metadata.get("imageUrl")}, ${dto.metadata.get("extrusionId").filterNot(_.isEmpty)}, ${dto.metadata.get("stickerId").filterNot(_.isEmpty)}, ${dto.tags.mkString(",")},
             |${dto.metadata.get("isKit").getOrElse("0")}, ${dto.isCustom.toInt}, ${dto.isEnabled.toInt},
             |${dto.createdAt}, ${dto.updatedAt})
             |""".stripMargin.update.withUniqueGeneratedKeys[Long]("id")

      for {
        descriptionId <- createDescription(dto.translations.toList) // Create description record
        productId <- insertProduct(descriptionId) // Persist product
        _ <- dto.storePrices.toList.traverse(createProductStorePrice(productId, _)) // Persist stores (@todo: Batch insert)
        _ <- dto.attributes.toList.traverse(createProductAttribute(productId, _)) // Persist attributes (@todo: Batch insert)
        _ <- dto.children.toList.traverse(createProductChild(productId, _)) // Persist children (@todo: Batch insert)
        _ <- dto.rules.toList.traverse(createProductRule(productId, _)) // Persist rules (@todo: Batch insert)
        _ <- dto.assemblyParts.toList.traverse(createProductAssemblyPart(productId, _)) // Persist assembly parts (@todo: Batch insert)
      } yield productId
    }

    def updateProduct(product: Product, dto: ProductDTO): ConnectionIO[Unit] = {
      val newHash = UUID.randomUUID().toString
      val updateProduct: Long => ConnectionIO[Unit] = (descriptionId: Long) =>
        sql"""UPDATE inv_products SET
             |sku = ${dto.sku}, hash = ${newHash}, description_id = $descriptionId, category_id = ${dto.categoryId}, department_id = ${dto.departmentId}, mpn = ${dto.metadata.get("mpn")}, retail_price = ${dto.price}, cost_price = ${dto.costPrice},
             |image_url = ${dto.metadata.get("imageUrl")}, extrusion_template_id = ${dto.metadata.get("extrusionId").filterNot(_.isEmpty)}, sticker_template_id = ${dto.metadata.get("stickerId").filterNot(_.isEmpty)}, tags = ${dto.tags.mkString(",")},
             |is_kit = ${dto.metadata.get("isKit").getOrElse("0")}, is_custom = ${dto.isCustom.toInt}, status = ${dto.isEnabled.toInt}, modification_date = ${dto.updatedAt})
             |""".stripMargin.update.run.map(_ => ())

      for {
        _ <- updateProduct(product.descriptionId) // Update product
        _ <- deleteTranslations(product.descriptionId) // Delete current translations
        _ <- deleteProductAttributes(product.id) // Delete current attributes
        _ <- deleteProductChildren(product.id) // Delete current children
        _ <- deleteProductRules(product.id) // Delete current rules

        _ <- dto.translations.toList.traverse(createTranslation(product.descriptionId, _)) // Persist translations (@todo: Batch insert)
        _ <- dto.attributes.toList.traverse(createProductAttribute(product.id, _)) // Persist attributes (@todo: Batch insert)
        _ <- dto.children.toList.traverse(createProductChild(product.id, _)) // Persist children (@todo: Batch insert)
        _ <- dto.rules.toList.traverse(createProductRule(product.id, _)) // Persist rules (@todo: Batch insert)

        // @todo: Handle product stores
        //        _ <- dto.assemblyParts.toList.traverse(createProductStorePrice(productId, _)) // Persist stores (@todo: Batch insert)
        //        _ <- deleteProductAssemblyParts(product.id) // Delete current children
      } yield ()
    }

    def createProductStorePrice(productId: Long, storePrice: ProductStorePriceDTO): ConnectionIO[Long] = {
      val sql = sql"INSERT INTO inv_product_stores (product_id, store_id, price) VALUES ($productId, ${storePrice.storeId}, ${storePrice.price})"

      sql.update.withUniqueGeneratedKeys[Long]("id")
    }

    def createProductAttribute(productId: Long, dto: ProductAttributeDTO): ConnectionIO[Long] = {
      val sql =
        sql"""INSERT INTO inv_product_attributes (product_id, attribute_id, attribute_value, is_reference, is_editable) VALUES
             |($productId, ${dto.attributeId}, ${dto.valueId.getOrElse(dto.value).toString}, ${dto.isReference}, ${dto.isEditable})""".stripMargin

      sql.update.withUniqueGeneratedKeys[Long]("id")
    }

    def updateProductAttribute(productId: Long, dto: ProductAttributeDTO): ConnectionIO[Boolean] = {
      val sql =
        sql"""UPDATE inv_product_attributes SET
             |attribute_value = ${dto.valueId.getOrElse(dto.value).toString}, is_reference = ${dto.isReference}, is_editable = ${dto.isEditable})
             |WHERE id = ${productId} AND attribute_id = ${dto.attributeId}""".stripMargin

      sql.update.run.map(_ > 0)
    }

    def createProductChild(productId: Long, dto: ProductChildDTO): ConnectionIO[Long] = {
      val sql =
        sql"""INSERT INTO inv_product_compositions (product_id, sub_product_id, type, quantity, is_compiled, is_visible) VALUES
             |($productId, ${dto.productId}, ${dto.childType}, ${dto.quantity}, ${dto.isCompiled}, ${dto.isVisible})""".stripMargin

      sql.update.withUniqueGeneratedKeys[Long]("id")
    }

    def createProductRule(productId: Long, dto: ProductRuleDTO): ConnectionIO[Long] = {
      val isOriginalPrice = dto.newPrice == 0.00
      val sql =
        sql"""INSERT INTO inv_product_relations (product_id, related_product_id, price, is_original_price, type, quantity, max_quantity) VALUES
             |($productId, ${dto.productId}, ${dto.newPrice}, ${isOriginalPrice}, ${dto.ruleType}, ${dto.quantity}, ${dto.maxAllowedQuantity})""".stripMargin

      sql.update.withUniqueGeneratedKeys[Long]("id")
    }

    def createProductAssemblyPart(productId: Long, dto: ProductAssemblyPartDTO): ConnectionIO[Long] = {
      val sql =
        sql"""INSERT INTO inv_product_assemblies (product_id, assembly_product_id, tag, is_default) VALUES
             |($productId, ${dto.productId}, ${dto.partType}, ${dto.isDefault})""".stripMargin

      sql.update.withUniqueGeneratedKeys[Long]("id")
    }

    def createDescription(translations: List[TranslationDTO]): ConnectionIO[Long] = {
      for {
        descriptionId <- sql"INSERT INTO descriptions (creation_date, modification_date) VALUES (NOW(), NOW())".update.withUniqueGeneratedKeys[Long]("id")
        _ <- translations.traverse(createTranslation(descriptionId, _))
      } yield descriptionId
    }

    def createTranslation(descriptionId: Long, dto: TranslationDTO): ConnectionIO[Long] = {
      val langId = Lang.fromString(dto.lang, 1)
      val sql =
        sql"""INSERT INTO translations
             |(description_id, lang_id, label, short_description, long_description, is_default) VALUES
             |($descriptionId, $langId, ${dto.name}, ${dto.shortDescription}, ${dto.longDescription}, ${dto.isDefault})""".stripMargin

      sql.update.withUniqueGeneratedKeys[Long]("id")
    }

    def deleteProduct(productId: Long): ConnectionIO[Boolean] =
      sql"DELETE FROM inv_products WHERE id = $productId".update.run.map(_ > 0)

    def deleteTranslations(descriptionId: Long): ConnectionIO[Int] =
      sql"DELETE FROM translations WHERE description_id = $descriptionId".update.run

    def deleteProductAttributes(productId: Long): ConnectionIO[Int] =
      sql"DELETE FROM inv_product_attributes WHERE product_id = $productId".update.run

    def deleteProductRules(productId: Long): ConnectionIO[Int] =
      sql"DELETE FROM inv_product_relations WHERE product_id = $productId".update.run

    def deleteProductChildren(productId: Long): ConnectionIO[Int] =
      sql"DELETE FROM inv_product_compositions WHERE product_id = $productId".update.run

    def deleteProductAssemblyParts(productId: Long): ConnectionIO[Int] =
      sql"DELETE FROM inv_product_assemblies WHERE product_id = $productId".update.run

  }

}
