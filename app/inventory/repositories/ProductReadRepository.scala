package inventory.repositories

import java.time.LocalDateTime
import javax.inject.Inject
import doobie._
import doobie.implicits._
import doobie.util.fragment.Fragment
import utils.imports.implicits._
import cats.data.OptionT
import cats.effect.{ContextShift, IO}
import cats.instances._
import cats.implicits._
import inventory.entities._
import shared.Types.{Product, Tx}
import shared.entities.Lang
import inventory.entities.ProductDB

import scala.collection.immutable.{ListSet, SortedSet}

final class ProductReadRepository @Inject()(db: Tx)(implicit cs: ContextShift[IO]) {

  def getById(id: Long, lang: String): IO[Option[Product]] =
    db.use(Queries.getProduct("id", id.toString, lang).transact(_))

  def getById(id: Long, lang: String, include: Seq[String] = Seq()): IO[Option[Product]] = {
    val program: ConnectionIO[Option[Product]] = (for {
      product <- OptionT(Queries.getProduct("id", id.toString, lang))
      productWithRelations <- OptionT.liftF(Queries.includeRelations(product, lang, include))
    } yield productWithRelations).value

    db.use(program.transact(_))
  }

  def getBySku(sku: String, lang: String): IO[Option[Product]] =
    db.use(Queries.getProduct("sku", sku, lang).transact(_))

  def getProductRules(productId: Long, lang: String): IO[List[ProductRule]] =
    db.use(Queries.getProductRules(productId, lang).transact(_))

  def getTranslations(descriptionId: Long): IO[List[Translation]] =
    db.use(Queries.getTranslations(descriptionId).transact(_))

  def getProductDepartment(deptId: Long, lang: String): IO[Option[ProductDepartment]] =
    db.use(Queries.getProductDepartment(deptId, lang).transact(_))

  def getProductDepartments(lang: String): IO[List[ProductDepartment]] =
    db.use(Queries.getProductDepartments(lang).transact(_))

  def handleInclusions(product: Product, lang: String, include: Seq[String]): IO[Product] =
    db.use(Queries.includeRelations(product, lang, include).transact(_))

  def getProductCategory(id: Long, lang: String): IO[Option[ProductCategory]] =
    db.use(Queries.getProductCategory(id, lang).transact(_))

  def getProductCategories(lang: String): IO[List[(ProductCategory, Int)]] =
    db.use(Queries.getProductCategories(lang).transact(_))

  def getAttribute(id: Long, lang: String): IO[Option[Attribute]] =
    db.use(Queries.getAttribute(id, lang).transact(_))

  def getAttributes(lang: String): IO[List[Attribute]] =
    db.use(Queries.getAttributes(lang).transact(_))

  def getAttributeValues(attributeId: Long, lang: String): IO[List[AttributeValue]] =
    db.use(Queries.getAttributeValues(attributeId, lang).transact(_))

  def getProductStorePrices(productId: Long): IO[List[ProductStorePrice]] =
    db.use(Queries.getProductStorePrices(productId).transact(_))

  def getProductAttributes(productId: Long, lang: String): IO[ListSet[ProductAttribute]] =
    db.use(Queries.getProductAttributes(productId, lang).transact(_))

  def getProductAttribute(productId: Long, attributeId: Long, lang: String): IO[Option[ProductAttribute]] =
    db.use(Queries.getProductAttribute(productId, attributeId, lang).transact(_))

  private object Queries {
    def getProduct(idType: String, id: String, lang: String): ConnectionIO[Option[Product]] = {
      val whereClause: Fragment = idType match {
        case "id" => fr"p.id = $id"
        case "sku" => fr"p.sku = $id"
        case _ => fr"p.id = $id"
      }

      val langId = Lang.fromString(lang)

      val sql: Fragment =
        fr"""
            SELECT
              p.id, p.sku, p.hash, p.description_id,
              COALESCE(t.label, dt.label) AS `p.name`,
              COALESCE(t.short_description, dt.short_description) AS `p.short_description`,
              COALESCE(t.long_description, dt.long_description) AS `p.long_description`,
              p.retail_price, p.cost_price, p.tags, p.is_custom, p.is_kit, p.status,
              p.mpn, p.image_url, p.sticker_template_id, p.extrusion_template_id,
              p.creation_date, p.modification_date,

              c.id, c.code,
              COALESCE(tc.label, dtc.label) AS `c.name`,
              COALESCE(tc.short_description, dtc.short_description) AS `c.short_description`,
              COALESCE(tc.long_description, dtc.long_description) AS `c.long_description`,
              c.creation_date, c.modification_date,

              d.id, d.code,
              COALESCE(td.label, dtd.label) AS `d.name`,
              COALESCE(td.short_description, dtd.short_description) AS `d.short_description`,
              COALESCE(td.long_description, dtd.long_description) AS `d.long_description`,
              d.creation_date, d.modification_date
            FROM inv_products p
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
            LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = $langId
            JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
            LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = $langId
            LEFT JOIN inv_departments d ON d.id = p.department_id
            LEFT JOIN translations dtd ON dtd.description_id = d.description_id AND dtd.is_default = 1
            LEFT JOIN translations td ON td.description_id = d.description_id AND td.lang_id = $langId
            WHERE
         """ ++ whereClause

      sql
        .query[ProductDB]
        .map(_.toEntity)
        .option
    }

    def getProductRules(productId: Long, lang: String): ConnectionIO[List[ProductRule]] = {
      // Fetch the rules
      val sql = sql"SELECT related_product_id, id, price, type, quantity, max_quantity FROM inv_product_relations WHERE product_id = $productId"
      val step1 = sql.query[(Long, Long, Double, String, Int, Int)].to[List]

      // Fetch each rule's corresponding product
      val step2: ConnectionIO[List[Option[ProductRule]]] = step1.flatMap { records =>
        records.traverse { rec =>
          val query: ConnectionIO[Option[Product]] = Queries.getProduct("id", rec._1.toString, lang)
          OptionT(query)
            .map(product => ProductRule(rec._2, product, rec._3, rec._4, rec._5, rec._6))
            .value
        }
      }

      step2.map(_.flatten)
    }

    def getProductDepartment(departmentId: Long, lang: String): ConnectionIO[Option[ProductDepartment]] = {
      val sql =
        sql"""
        SELECT
          d.id, d.code,
          COALESCE(t.label, dt.label) AS `d.name`,
          COALESCE(t.short_description, dt.short_description) AS `d.short_description`,
          COALESCE(t.long_description, dt.long_description) AS `d.long_description`,
          d.creation_date, d.modification_date
        FROM inv_departments d
        JOIN translations dt ON dt.description_id = d.description_id AND dt.is_default = 1
        LEFT JOIN translations t ON t.description_id = d.description_id AND t.lang_id = $lang
        WHERE d.id = $departmentId
      """

      sql
        .query[(Long, String, Description, LocalDateTime, Option[LocalDateTime])]
        .map(rec => ProductDepartment(rec._1, rec._2, rec._3, rec._4, rec._5))
        .option
    }

    def getProductDepartments(lang: String): ConnectionIO[List[ProductDepartment]] = {
      val sql =
        sql"""
        SELECT
          d.id, d.code,
          COALESCE(t.label, dt.label) AS `d.name`,
          COALESCE(t.short_description, dt.short_description) AS `d.short_description`,
          COALESCE(t.long_description, dt.long_description) AS `d.long_description`,
          d.creation_date, d.modification_date
        FROM inv_departments d
        JOIN translations dt ON dt.description_id = d.description_id AND dt.is_default = 1
        LEFT JOIN translations t ON t.description_id = d.description_id AND t.lang_id = $lang
      """

      sql.query[ProductDepartment].to[List]
    }

    def getTranslations(descriptionId: Long): ConnectionIO[List[Translation]] = {
      val sql = sql"SELECT t.id, l.code, t.label, t.short_description, t.long_description, is_default FROM translations t JOIN languages l ON l.id = t.lang_id WHERE t.description_id = $descriptionId"

      sql.query[Translation].to[List]
    }

    def getProductCategory(categoryId: Long, lang: String): ConnectionIO[Option[ProductCategory]] = {
      val langId = Lang.fromString(lang)
      val sql =
        sql"""
         SELECT
           c.id, c.code,
          COALESCE(t.label, dt.label),
          COALESCE(t.short_description, dt.short_description),
          COALESCE(t.long_description, dt.long_description),
          c.creation_date, c.modification_date
         FROM inv_product_categories AS c
         JOIN translations dt ON dt.description_id = c.description_id AND dt.is_default = 1
         LEFT JOIN translations t ON t.description_id = c.description_id AND t.lang_id = $langId
         WHERE c.id = $categoryId
       """

      val step1 = sql
        .query[(Long, String, Description, LocalDateTime, Option[LocalDateTime])]
        .map(ProductCategory.tupled(_))
        .option

      val step2 = for {
        pc <- OptionT(step1)
        parents <- OptionT.liftF(getCategoryParents(pc.id))
      } yield pc.copy(parents = parents)

      step2.value
    }

    def getProductCategories(lang: String): ConnectionIO[List[(ProductCategory, Int)]] = {
      val langId = Lang.fromString(lang, 1)
      val sql =
        sql"""
        SELECT
          c.id, c.code,
          COALESCE(t.label, dt.label),
          COALESCE(t.short_description, dt.short_description),
          COALESCE(t.long_description, dt.long_description),
          c.creation_date, c.modification_date,
          IF(c.left_id = 1, 1, COUNT(parent.id)) AS depth
        FROM inv_product_categories c
          JOIN inv_product_categories parent ON c.left_id BETWEEN parent.left_id AND parent.right_id
          JOIN translations dt ON dt.description_id = c.description_id AND dt.is_default = 1
          LEFT JOIN translations t ON t.description_id = c.description_id AND t.lang_id = $langId
        GROUP BY c.id
        ORDER BY c.left_id
      """

      sql
        .query[(Long, String, Description, LocalDateTime, Option[LocalDateTime], Int)]
        .map(t => (ProductCategory(t._1, t._2, t._3, t._4, t._5), t._6))
        .to[List]
    }

    def getCategoryParents(categoryId: Long): ConnectionIO[SortedSet[String]] = {
      val sql = sql"SELECT DISTINCT parent.code FROM inv_product_categories actual JOIN inv_product_categories parent ON parent.left_id < actual.left_id AND parent.right_id > actual.right_id WHERE actual.id = $categoryId ORDER BY parent.left_id DESC"

      sql.query[String].to[SortedSet]
    }

    def getProductAttribute(productId: Long, attributeId: Long, lang: String): ConnectionIO[Option[ProductAttribute]] = {
      val langId = Lang.fromString(lang, 1)
      val sql =
        sql"""
        SELECT
         	pa.id,
         	a.id, a.code, data_types.code, input_types.code,
         	COALESCE(t.label, dt.label),
         	COALESCE(t.short_description, dt.short_description),
         	COALESCE(t.long_description, dt.long_description),
         	COALESCE(COALESCE(tv.label, dtv.label), pa.attribute_value),
          a.creation_date, a.modification_date,
         	pa.attribute_value, v.id, v.sku,
          pa.is_editable, pa.is_reference
         FROM inv_product_attributes AS pa
         	JOIN inv_attributes a ON a.id = pa.attribute_id
         	JOIN data_types ON data_types.id = a.data_type_id
         	JOIN input_types ON input_types.id = a.input_type_id
         	JOIN translations dt ON dt.description_id = a.description_id AND dt.is_default = 1
         	LEFT JOIN translations t ON t.description_id = a.description_id AND t.lang_id = $langId
         	LEFT JOIN inv_values v ON v.id = pa.attribute_value AND pa.is_reference = 1
         	LEFT JOIN translations tv ON tv.description_id = v.description_id AND tv.lang_id = $langId
         	LEFT JOIN translations dtv ON dtv.description_id = v.description_id AND dtv.is_default = 1
         WHERE pa.product_id = $productId AND pa.attribute_id = $attributeId
       """

      sql
        .query[(Long, Long, String, String, String, Description, LocalDateTime, Option[LocalDateTime], String, Option[Long], Option[String], Boolean, Boolean)]
        .map(rec => ProductAttribute(
          rec._1, Attribute(
            rec._2,
            rec._3,
            rec._4,
            rec._5,
            rec._6,
            rec._7,
            rec._8
          ),
          rec._9,
          rec._10,
          rec._11,
          rec._12,
          rec._13,
        ))
        .option
    }

    def getProductAttributes(productId: Long, lang: String): ConnectionIO[ListSet[ProductAttribute]] = {
      val langId = Lang.fromString(lang, 1)
      val sql =
        sql"""
        SELECT
         	pa.id,
         	a.id, a.code, data_types.code, input_types.code,
         	COALESCE(t.label, dt.label),
         	COALESCE(t.short_description, dt.short_description),
         	COALESCE(t.long_description, dt.long_description),
         	COALESCE(COALESCE(tv.label, dtv.label), pa.attribute_value),
          a.creation_date, a.modification_date,
         	pa.attribute_value, v.id, v.sku,
          pa.is_editable, pa.is_reference
         FROM inv_product_attributes AS pa
         	JOIN inv_attributes a ON a.id = pa.attribute_id
         	JOIN data_types ON data_types.id = a.data_type_id
         	JOIN input_types ON input_types.id = a.input_type_id
         	JOIN translations dt ON dt.description_id = a.description_id AND dt.is_default = 1
         	LEFT JOIN translations t ON t.description_id = a.description_id AND t.lang_id = $langId
         	LEFT JOIN inv_values v ON v.id = pa.attribute_value AND pa.is_reference = 1
         	LEFT JOIN translations tv ON tv.description_id = v.description_id AND tv.lang_id = $langId
         	LEFT JOIN translations dtv ON dtv.description_id = v.description_id AND dtv.is_default = 1
         WHERE pa.product_id = $productId
         ORDER BY pa.id
       """

      sql
        .query[(Long, Long, String, String, String, Description, LocalDateTime, Option[LocalDateTime], String, Option[Long], Option[String], Boolean, Boolean)]
        .map(rec => ProductAttribute(
          rec._1, Attribute(
            rec._2,
            rec._3,
            rec._4,
            rec._5,
            rec._6,
            rec._7,
            rec._8
          ),
          rec._9,
          rec._10,
          rec._11,
          rec._12,
          rec._13,
        ))
        .to[ListSet]
    }

    def getAttribute(attributeId: Long, lang: String): ConnectionIO[Option[Attribute]] = {
      val langId = Lang.fromString(lang, 1)
      val sql =
        sql"""
        SELECT
         	a.id, a.code, data_types.code, input_types.code,
         	COALESCE(t.label, dt.label) AS label,
         	COALESCE(t.short_description, dt.short_description) AS short_description,
         	COALESCE(t.long_description, dt.long_description) AS long_description,
          a.creation_date, a.modification_date
         FROM inv_attributes a
         	JOIN data_types ON data_types.id = a.data_type_id
         	JOIN input_types ON input_types.id = a.input_type_id
         	JOIN translations dt ON dt.description_id = a.description_id AND dt.is_default = 1
         	LEFT JOIN translations t ON t.description_id = a.description_id AND t.lang_id = $langId
         WHERE a.id = $attributeId
       """

      sql
        .query[(Long, String, String, String, Description, LocalDateTime, Option[LocalDateTime])]
        .map(Attribute.tupled(_))
        .option
    }

    def getAttributes(lang: String): ConnectionIO[List[Attribute]] = {
      val sql =
        sql"""
        SELECT
         	a.id, a.code, data_types.code, input_types.code,
         	COALESCE(t.label, dt.label) AS label,
         	COALESCE(t.short_description, dt.short_description) AS short_description,
         	COALESCE(t.long_description, dt.long_description) AS long_description,
          a.creation_date, a.modification_date
         FROM inv_attributes a
         	JOIN data_types ON data_types.id = a.data_type_id
         	JOIN input_types ON input_types.id = a.input_type_id
         	JOIN translations dt ON dt.description_id = a.description_id AND dt.is_default = 1
         	LEFT JOIN translations t ON t.description_id = a.description_id AND t.lang_id = ${Lang.fromString(lang, 1)}
         ORDER BY label ASC
       """

      sql
        .query[(Long, String, String, String, Description, LocalDateTime, Option[LocalDateTime])]
        .map(Attribute.tupled(_))
        .to[List]
    }

    def getAttributeValues(attributeId: Long, lang: String): ConnectionIO[List[AttributeValue]] = {
      val sql =
        sql"""
         SELECT
          v.id, v.sku,
          COALESCE(t.label, dt.label),
          COALESCE(t.short_description, dt.short_description),
          COALESCE(t.long_description, dt.long_description)
         FROM inv_attribute_values AS av
         JOIN inv_values v ON v.id = av.value_id
         JOIN translations dt ON dt.description_id = v.description_id AND dt.is_default = 1
         LEFT JOIN translations t ON t.description_id = v.description_id AND t.lang_id = @langId
         WHERE av.attribute_id = @attributeId
       """

      sql.query[AttributeValue].to[List]
    }

    def getProductStorePrices(productId: Long): ConnectionIO[List[ProductStorePrice]] = {
      val sql = sql"SELECT store_id, price FROM inv_product_stores WHERE product_id = $productId"

      sql.query[ProductStorePrice].to[List]
    }

    def includeRelations(product: Product, lang: String, include: Seq[String]): ConnectionIO[Product] = {
      include.foldLeft(product.pure[ConnectionIO]) { (acc, code) =>
        code match {
          //      case ProductInclusions.ATTRIBUTES =>
          //        getProductAttributes(product.id, lang).map { data =>
          //          (s, product.copy(attributes = data))
          //        }
          //      case ProductInclusions.CHILDREN =>
          //        getProductChildren(product.id, lang).map { data =>
          //          (s, product.copy(children = data))
          //        }
          //      case ProductInclusions.ASSEMBLY_PARTS =>
          //        getProductAssemblyParts(product.id, lang).map { data =>
          //          (s, product.copy(assemblyParts = data))
          //        }
          case ProductInclusions.RULES =>
            for {
              rules <- Queries.getProductRules(product.id, lang)
              prod <- acc
            } yield prod.copy(rules = rules)

          case _ => acc
        }
      }
    }

  }

}
