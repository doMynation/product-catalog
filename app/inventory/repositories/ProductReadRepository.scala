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
import inventory.util.{SearchRequest, SearchResult}
import scala.collection.immutable.{ListSet, SortedSet}

final class ProductReadRepository @Inject()(db: Tx)(implicit cs: ContextShift[IO]) {

  def getById(id: Long, lang: String): IO[Option[Product]] =
    db.use(Queries.getProduct("id", id.toString, lang).transact(_))

  def getById(id: Long, lang: String, include: Seq[String] = List()): IO[Option[Product]] =
    db.use(Queries.getProductWithRelations("id", id.toString, lang).transact(_))

  def getBySku(sku: String, lang: String): IO[Option[Product]] =
    db.use(Queries.getProduct("sku", sku, lang).transact(_))

  def getBySku(sku: String, lang: String, include: Seq[String]): IO[Option[Product]] =
    db.use(Queries.getProductWithRelations("sku", sku, lang).transact(_))

  def getProductRule(ruleId: Long, lang: String): IO[Option[ProductRule]] =
    db.use(Queries.getProductRule(ruleId, lang).transact(_))

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

  def getAttributeValue(valueId: Long, lang: String): IO[Option[AttributeValue]] =
    db.use(Queries.getAttributeValue(valueId, lang).transact(_))

  def getAttributeValues(attributeId: Long, lang: String): IO[List[AttributeValue]] =
    db.use(Queries.getAttributeValues(attributeId, lang).transact(_))

  def getProductStorePrices(productId: Long): IO[List[ProductStorePrice]] =
    db.use(Queries.getProductStorePrices(productId).transact(_))

  def getProductAttributes(productId: Long, lang: String): IO[ListSet[ProductAttribute]] =
    db.use(Queries.getProductAttributes(productId, lang).transact(_))

  def getProductAttribute(productId: Long, attributeId: Long, lang: String): IO[Option[ProductAttribute]] =
    db.use(Queries.getProductAttribute(productId, attributeId, lang).transact(_))

  def search(sr: SearchRequest, lang: String, include: Seq[String] = Seq()): IO[SearchResult[Product]] =
    db.use(Queries.search(sr, lang, include).transact(_))

  def applyProductAttributeOverrides(product: Product, attributeOverrides: Seq[(String, String)], lang: String): IO[Product] = {
    val overridenAttributes: List[(ProductAttribute, String)] = attributeOverrides.toList.flatMap(tuple => product.getAttribute(tuple._1) match {
      case Some(productAttribute) => Some(productAttribute, tuple._2)
      case _ => None
    })

    val productWithOverridenAttributes = overridenAttributes.foldLeft(IO.pure(product)) {
      (acc, tuple) =>
        (tuple._1, tuple._2, tuple._1.attribute.inputType) match {
          case (productAttribute, newValue, "select") =>
            val program: OptionT[IO, Product] = for {
              p <- OptionT.liftF(acc)
              av <- OptionT(getAttributeValue(newValue.toLong, lang))
            } yield p.replaceAttribute(productAttribute, productAttribute.copy(
              value = av.description.name,
              valueId = Some(av.id),
              valueSku = Some(av.sku)
            ))

            program.getOrElseF(acc)
          case (productAttribute, newValue, _) =>
            acc.map(_.replaceAttribute(
              productAttribute,
              productAttribute.copy(value = newValue))
            )
        }
    }

    productWithOverridenAttributes
  }

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
            WHERE """ ++ whereClause

      sql
        .query[ProductDB]
        .map(_.toEntity)
        .option
    }

    def getProductWithRelations(idType: String, id: String, lang: String, include: Seq[String] = List()): ConnectionIO[Option[Product]] = {
      (for {
        product <- OptionT(Queries.getProduct(idType, id, lang))
        productWithRelations <- OptionT.liftF(Queries.includeRelations(product, lang, include))
      } yield productWithRelations).value
    }

    def getProductRule(ruleId: Long, lang: String): ConnectionIO[Option[ProductRule]] = {
      val sql = sql"SELECT related_product_id, id, price, type, quantity, max_quantity FROM inv_product_relations WHERE id = $ruleId"
      val step1 = sql.query[(Long, Long, Double, String, Int, Int)].option

      val step2: OptionT[ConnectionIO, ProductRule] = for {
        rec <- OptionT(step1)
        product <- OptionT(getProduct("id", rec._1.toString, lang))
      } yield ProductRule(rec._2, product, rec._3, rec._4, rec._5, rec._6)

      step2.value
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

    def getProductChildren(productId: Long, lang: String): ConnectionIO[List[ProductChild]] = {
      // Fetch the children
      val sql = sql"SELECT sub_product_id, id, type, quantity, is_compiled, is_visible FROM inv_product_compositions WHERE product_id = $productId"
      val step1 = sql.query[(Long, Long, String, Int, Boolean, Boolean)].to[List]

      // Fetch each child's corresponding product
      val step2: ConnectionIO[List[Option[ProductChild]]] = step1.flatMap { records =>
        records.traverse { rec =>
          val query: ConnectionIO[Option[Product]] = Queries.getProduct("id", rec._1.toString, lang)
          OptionT(query)
            .map(product => ProductChild(rec._2, product, rec._3, rec._4, rec._5, rec._6))
            .value
        }
      }

      step2.map(_.flatten)
    }

    def getProductAssemblyParts(productId: Long, lang: String): ConnectionIO[List[ProductAssemblyPart]] = {
      // Fetch the assembly parts
      val sql = sql"SELECT assembly_product_id, tag, is_default FROM inv_product_assemblies WHERE product_id = $productId"
      val step1 = sql.query[(Long, String, Boolean)].to[List]

      // Fetch each child's corresponding product
      val step2: ConnectionIO[List[Option[ProductAssemblyPart]]] = step1.flatMap { records =>
        records.traverse { rec =>
          val query: ConnectionIO[Option[Product]] = Queries.getProduct("id", rec._1.toString, lang)
          OptionT(query)
            .map(product => ProductAssemblyPart(product, rec._2, rec._3))
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

    def getAttributeValue(valueId: Long, lang: String): ConnectionIO[Option[AttributeValue]] = {
      val langId = Lang.fromString(lang, 1)
      val sql =
        sql"""
        SELECT
          v.id, v.sku,
          COALESCE(t.label, dt.label),
          COALESCE(t.short_description, dt.short_description),
          COALESCE(t.long_description, dt.long_description),
          v.creation_date, v.modification_date
        FROM inv_values v
        JOIN translations dt ON dt.description_id = v.description_id AND dt.is_default = 1
        LEFT JOIN translations t ON t.description_id = v.description_id AND t.lang_id = $langId
        WHERE v.id = $valueId
       """

      sql.query[AttributeValue].option
    }

    def getAttributeValues(attributeId: Long, lang: String): ConnectionIO[List[AttributeValue]] = {
      val langId = Lang.fromString(lang, 1)
      val sql =
        sql"""
         SELECT
          v.id, v.sku,
          COALESCE(t.label, dt.label),
          COALESCE(t.short_description, dt.short_description),
          COALESCE(t.long_description, dt.long_description),
          v.creation_date, v.modification_date
         FROM inv_attribute_values AS av
         JOIN inv_values v ON v.id = av.value_id
         JOIN translations dt ON dt.description_id = v.description_id AND dt.is_default = 1
         LEFT JOIN translations t ON t.description_id = v.description_id AND t.lang_id = $langId
         WHERE av.attribute_id = $attributeId
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
          case ProductInclusions.ATTRIBUTES =>
            for {
              attrs <- Queries.getProductAttributes(product.id, lang)
              prod <- acc
            } yield prod.copy(attributes = attrs)
          case ProductInclusions.CHILDREN =>
            for {
              children <- Queries.getProductChildren(product.id, lang)
              prod <- acc
            } yield prod.copy(children = children)
          case ProductInclusions.ASSEMBLY_PARTS =>
            for {
              parts <- Queries.getProductAssemblyParts(product.id, lang)
              prod <- acc
            } yield prod.copy(assemblyParts = parts)
          case ProductInclusions.RULES =>
            for {
              rules <- Queries.getProductRules(product.id, lang)
              prod <- acc
            } yield prod.copy(rules = rules)

          case _ => acc
        }
      }
    }

    def search(sr: SearchRequest, lang: String, include: Seq[String] = Seq()): ConnectionIO[SearchResult[Product]] = {
      val langId = Lang.fromString(lang, 1)
      val wheres: List[Option[Fragment]] = List(
        Some(fr"1=1"),
        sr.filters.get("id").map(value => fr"p.id LIKE ${"%" + value + "%"}"),
        sr.filters.get("sku").map(value => fr"p.sku LIKE ${"%" + value + "%"}"),
        sr.filters.get("isEnabled").map(value => fr"p.status = $value"),
        sr.filters.get("isKit").map(value => fr"p.is_kit = $value"),
        sr.filters.get("isCustom").map(value => fr"p.is_custom = $value"),
        sr.filters.get("department").map(value => fr"d.code = $value"),
        sr.filters.get("departmentId").map(value => fr"d.id = $value"),
      )

      val havings: List[Option[Fragment]] = List(
        Some(fr"1=1"),
        sr.filters.get("name").map(value => fr"p.name LIKE ${"%" + value + "%"}"),
        sr.filters.get("label").map(value => fr"p.name LIKE ${"%" + value + "%"}"), // alias of `name` filter
        sr.filters.get("nameSku").map(value => fr"(`p.name` LIKE ${"%" + value + "%"} OR sku LIKE ${"%" + value + "%"})"),
      )

      val joins = List(
        sr.filters.get("storeId").map(value => {
          fr"JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = $value"
        }),
        sr.filters.get("category").map(value => {
          val categories = value.split(",").toList.toNel

          fr"JOIN inv_product_categories tree ON " ++
            Fragments.in(fr"tree.code", categories.get) ++
            fr"AND tree.left_id <= c.left_id AND tree.right_id >= c.right_id"
        }),
        sr.filters.get("categoryId").map(value => {
          val categories = value.split(",").toList.toNel

          fr"JOIN inv_product_categories tree ON " ++
            Fragments.in(fr"tree.id", categories.get) ++
            fr"AND tree.left_id <= c.left_id AND tree.right_id >= c.right_id"
        }),
      )

      val allowedSortFields = Map(
        "id" -> "p.id",
        "sku" -> "sku",
        "name" -> "`p.name`",
        "price" -> "retail_price",
        "shortDescription" -> "`p.short_description`",
        "longDescription" -> "`p.long_description`",
        "isCustom" -> "p.is_custom",
        "category" -> "p.category_id",
      )

      // Default sort to ID in descending order
      val (sortField, sortOrder) = sr.sortField
        .flatMap(allowedSortFields.get)
        .map(field => (field, sr.sortOrder))
        .getOrElse(("p.id", SearchRequest.SORT_DESC))

      val limitClause = sr.limit
        .map(lim => fr"LIMIT ${sr.offset}, $lim")
        .getOrElse(fr"LIMIT 100")

      val sql: Fragment =
        fr"""
              SELECT
                SQL_CALC_FOUND_ROWS
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
              LEFT JOIN translations td ON td.description_id = d.description_id AND td.lang_id = $langId""" ++
          joins.flatten.intercalate(fr" ") ++
          Fragments.whereAndOpt(wheres: _*) ++
          fr"HAVING " ++ Fragments.andOpt(havings: _*) ++
          Fragment.const(s"ORDER BY $sortField $sortOrder") ++
          limitClause

      val step1: ConnectionIO[List[Product]] = sql
        .query[ProductDB]
        .map(_.toEntity)
        .to[List]

      for {
        products <- step1 // Fetch all products
        count <- sql"""SELECT FOUND_ROWS()""".query[Int].unique // Get the total number of records
        productsWithRelations <- products.traverse(includeRelations(_, lang, include)) // Handle includes for each product
      } yield SearchResult(productsWithRelations, count)
    }
  }

}
