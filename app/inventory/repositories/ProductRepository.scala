package inventory.repositories

import cats.data.OptionT
import cats.implicits._
import infra.DatabaseExecutionContext
import inventory.entities._
import inventory.util.{DB, SearchRequest, SearchResult}
import javax.inject.Inject
import play.api.db.Database
import shared.Types.Product
import shared.entities.Lang
import scala.collection.immutable.{ListSet, Queue, SortedSet}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class ProductRepository @Inject()(db: Database)(implicit dec: DatabaseExecutionContext) {

  def getById(id: Long, lang: String): Future[Option[Product]] =
    getProduct("id", id.toString, lang)

  def getById(id: Long, lang: String, include: Seq[String] = Seq()): Future[Option[Product]] = {
    val program = for {
      product <- OptionT(getById(id, lang))
      product <- OptionT.liftF(handleInclusions(product, lang, include))
    } yield product

    program.value
  }

  def getBySku(sku: String, lang: String): Future[Option[Product]] =
    getProduct("sku", sku, lang)

  def getBySku(sku: String, lang: String, include: Seq[String] = Seq()): Future[Option[Product]] = {
    val program = for {
      product <- OptionT(getBySku(sku, lang))
      product <- OptionT.liftF(handleInclusions(product, lang, include))
    } yield product

    program.value
  }

  private def getProduct(idType: String, id: String, lang: String): Future[Option[Product]] = Future {
    val whereClause: String = idType match {
      case "id" => "p.id = @id"
      case "sku" => "p.sku = @id"
      case _ => "p.id = @id"
    }

    val sql =
      s"""
            SELECT
              p.*,
              p.retail_price AS price,
              c.*,
              d.*,
              COALESCE(t.label, dt.label) AS `p.name`,
              COALESCE(t.short_description, dt.short_description) AS `p.short_description`,
              COALESCE(t.long_description, dt.long_description) AS `p.long_description`,
              COALESCE(tc.label, dtc.label) AS `c.name`,
              COALESCE(tc.short_description, dtc.short_description) AS `c.short_description`,
              COALESCE(tc.long_description, dtc.long_description) AS `c.long_description`,
              COALESCE(td.label, dtd.label) AS `d.name`,
              COALESCE(td.short_description, dtd.short_description) AS `d.short_description`,
              COALESCE(td.long_description, dtd.long_description) AS `d.long_description`
            FROM inv_products p
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
            LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = @langId
            JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
            LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = @langId
            LEFT JOIN inv_departments d ON d.id = p.department_id
            LEFT JOIN translations dtd ON dtd.description_id = d.description_id AND dtd.is_default = 1
            LEFT JOIN translations td ON td.description_id = d.description_id AND td.lang_id = @langId
            WHERE $whereClause
         """

    val query = DB.fetchOne(sql, Map(
      "id" -> id,
      "langId" -> Lang.fromString(lang, 1).toString
    ))(Hydrators.hydrateProduct) _

    db.withConnection(query)
  }

  def getAttributeValue(valueId: Long, lang: String): Future[Option[AttributeValue]] = Future {
    val sql =
      s"""
         SELECT
           v.*,
          COALESCE(t.label, dt.label) AS name,
          COALESCE(t.short_description, dt.short_description) AS short_description,
          COALESCE(t.long_description, dt.long_description) AS long_description
         FROM inv_values v
         JOIN translations dt ON dt.description_id = v.description_id AND dt.is_default = 1
         LEFT JOIN translations t ON t.description_id = v.description_id AND t.lang_id = @langId
         WHERE v.id = @valueId
       """

    val query = DB.fetchOne(sql, Map(
      "langId" -> Lang.fromString(lang, 1).toString,
      "valueId" -> valueId.toString
    ))(Hydrators.hydrateAttributeValue) _

    db.withConnection(query)
  }

  def getAttributeValues(attributeId: Long, lang: String): Future[Seq[AttributeValue]] = Future {
    val sql =
      s"""
         SELECT
           v.*,
          COALESCE(t.label, dt.label) AS name,
          COALESCE(t.short_description, dt.short_description) AS short_description,
          COALESCE(t.long_description, dt.long_description) AS long_description
         FROM inv_attribute_values AS av
         JOIN inv_values v ON v.id = av.value_id
         JOIN translations dt ON dt.description_id = v.description_id AND dt.is_default = 1
         LEFT JOIN translations t ON t.description_id = v.description_id AND t.lang_id = @langId
         WHERE av.attribute_id = @attributeId
       """

    val query = DB.fetchMany(sql, Map(
      "langId" -> Lang.fromString(lang, 1).toString,
      "attributeId" -> attributeId.toString
    ))(Hydrators.hydrateAttributeValue) _

    db.withConnection(query)
  }

  def getProductRules(productId: Long, lang: String): Future[Seq[ProductRule]] = {
    // Fetch the records
    val step1 = Future {
      val sql = "SELECT * FROM inv_product_relations WHERE product_id = @productId"
      val query = DB.fetchMany(sql, Map("productId" -> productId.toString))(Hydrators.productRuleExtractor) _

      db.withConnection(query)
    }

    val step2 = step1.flatMap { records =>
      Future.traverse(records) { rec =>
        (for {
          product <- OptionT(getById(rec._1, lang))
        } yield ProductRule(rec._2, product, rec._3, rec._4, rec._5, rec._6)).value
      }
    }

    step2.map(_.flatten)
  }

  def getProductRule(id: Long, lang: String): Future[Option[ProductRule]] = {
    val step1 = Future {
      val sql = "SELECT * FROM inv_product_relations pr WHERE pr.id = @ruleId"
      val query = DB.fetchOne(sql, Map("ruleId" -> id.toString))(Hydrators.productRuleExtractor) _

      db.withConnection(query)
    }

    val step2 = for {
      rec <- OptionT(step1)
      product <- OptionT(getById(rec._1, lang))
    } yield ProductRule(rec._2, product, rec._3, rec._4, rec._5, rec._6)

    step2.value
  }

  def getTranslations(descriptionId: Long): Future[Seq[Translation]] = Future {
    val sql = "SELECT * FROM translations WHERE description_id = @descriptionId"
    val query = DB.fetchMany(sql, Map("descriptionId" -> descriptionId.toString))(Hydrators.hydrateTranslation) _

    db.withConnection(query)
  }

  def getDescription(id: Long, lang: String): Future[Option[Description]] = Future {
    val sql = "SELECT t.* FROM translations AS t JOIN languages l ON l.id = t.lang_id AND t.code = @lang WHERE description_id = @descriptionId"
    val query = DB.fetchOne(sql, Map("lang" -> lang, "descriptionId" -> id.toString))(Hydrators.hydrateDescription) _

    db.withConnection(query)
  }

  def search(sr: SearchRequest, lang: String, include: Seq[String] = Seq()): Future[SearchResult[Product]] = {
    val wheres = new ListBuffer[String]()
    val havings = new ListBuffer[String]()
    val joins = new ListBuffer[String]()
    var params: Map[String, String] = Map(
      "langId" -> Lang.fromString(lang, 1).toString
    )

    wheres += "1 = 1"
    havings += "1 = 1"

    sr.filters.get("isEnabled").foreach(value => {
      wheres += "p.status = @isEnabled"
      params = params + ("isEnabled" -> value)
    })

    sr.filters.get("isKit").foreach(value => {
      wheres += "p.is_kit = @isKit"
      params = params + ("isKit" -> value)
    })

    sr.filters.get("isCustom").foreach(value => {
      wheres += "is_custom = @isCustom"
      params = params + ("isCustom" -> value)
    })

    // `id` filter
    sr.filters.get("id").foreach(value => {
      wheres += "p.id LIKE @id"
      params = params + ("id" -> s"%$value%")
    })

    // `sku` filter
    sr.filters.get("sku").foreach(value => {
      wheres += "sku LIKE @sku"
      params = params + ("sku" -> s"%$value%")
    })

    // `name` filter
    sr.filters.get("name").foreach(value => {
      havings += "`p.name` LIKE @name"
      params = params + ("name" -> s"%$value%")
    })

    // `label` filter (alias for `name`)
    sr.filters.get("label").foreach(value => {
      havings += "`p.name` LIKE @name"
      params = params + ("name" -> s"%$value%")
    })

    // `nameSku` filter
    sr.filters.get("nameSku").foreach(value => {
      havings += "(`p.name` LIKE @name OR sku LIKE @sku)"
      params = params + ("name" -> s"%$value%", "sku" -> s"%$value%")
    })

    // `department` filter
    sr.filters.get("department").foreach(value => {
      wheres += "(d.code = @department)"
      params = params + ("department" -> value)
    })

    sr.filters.get("departmentId").foreach(value => {
      wheres += "(d.id = @departmentId)"
      params = params + ("departmentId" -> value)
    })

    // `category` filter (e.g. "model", "model,option")
    sr.filters.get("category").foreach(value => {
      val categories = value.split(",")
      val inClause = categories.foldLeft(Queue[String]()) {
        (acc, categoryName) => acc :+ s"'$categoryName'"
      }.mkString(",")

      joins += s"JOIN inv_product_categories tree ON tree.code IN ($inClause) AND tree.left_id <= c.left_id AND tree.right_id >= c.right_id"
    })

    sr.filters.get("categoryId").foreach(value => {
      val categories = value.split(",")
      val inClause = categories.foldLeft(Queue[String]()) {
        (acc, categoryId) => acc :+ s"$categoryId"
      }.mkString(",")

      joins += s"JOIN inv_product_categories tree ON tree.id IN ($inClause) AND tree.left_id <= c.left_id AND tree.right_id >= c.right_id"
    })

    sr.filters.get("storeId").foreach(value => {
      joins += "JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId"
      params = params + ("storeId" -> value.toString)
    })

    val allowedSortFields = Map(
      "id" -> "p.id",
      "sku" -> "sku",
      "name" -> "`p.name`",
      "price" -> "price",
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

    val fetchSql =
      s"""
              SELECT
                SQL_CALC_FOUND_ROWS
                p.*,
                c.*,
                d.*,
                p.retail_price AS price,
                COALESCE(t.label, dt.label) AS `p.name`,
                COALESCE(t.short_description, dt.short_description) AS `p.short_description`,
                COALESCE(t.long_description, dt.long_description) AS `p.long_description`,
                COALESCE(tc.label, dtc.label) AS `c.name`,
                COALESCE(tc.short_description, dtc.short_description) AS `c.short_description`,
                COALESCE(tc.long_description, dtc.long_description) AS `c.long_description`,
                COALESCE(td.label, dtd.label) AS `d.name`,
                COALESCE(td.short_description, dtd.short_description) AS `d.short_description`,
                COALESCE(td.long_description, dtd.long_description) AS `d.long_description`
              FROM inv_products p
              JOIN inv_product_categories c ON c.id = p.category_id
              JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
              LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = @langId
              JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
              LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = @langId
              LEFT JOIN inv_departments d ON d.id = p.department_id
              LEFT JOIN translations dtd ON dtd.description_id = d.description_id AND dtd.is_default = 1
              LEFT JOIN translations td ON td.description_id = d.description_id AND td.lang_id = @langId
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
              ORDER BY $sortField ${sortOrder}
              ${sr.limit.map(lim => s"LIMIT ${sr.offset}, $lim").getOrElse("LIMIT 100")}
        """

    // Get the products
    val step1 = Future {
      db.withConnection { conn =>
        val products = DB.fetchMany(fetchSql, params)(Hydrators.hydrateProduct)(conn)
        val totalCount = DB.fetchColumn[Int]("SELECT FOUND_ROWS()")(conn)

        (products, totalCount.get)
      }
    }

    // Handle includes for all products
    val step2 = step1.flatMap { tuple =>
      val productsWithInclude = Future.traverse(tuple._1) { s =>
        println(Thread.currentThread.getName)
        handleInclusions(s, lang, include)
      }
      productsWithInclude.map((_, tuple._2))
    }

    // Return a SearchResult
    step2.map(result => SearchResult(result._1, result._2))
  }

  def getProductAssemblyParts(productId: Long, lang: String): Future[Seq[ProductAssemblyPart]] = {
    val sql = "SELECT * FROM inv_product_assemblies WHERE product_id = @productId"

    // Get all records
    val step1 = Future {
      val query = DB.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
        (rs.getLong("assembly_product_id"), rs.getString("tag"), rs.getBoolean("is_default"))
      } _

      db.withConnection(query)
    }

    // Fetch their corresponding product
    val step2 = step1.flatMap { records =>
      Future.traverse(records) { rec =>
        OptionT(getById(rec._1, lang))
          .map(ProductAssemblyPart(_, rec._2, rec._3)).value
      }
    }

    step2.map(_.flatten)
  }

  def getProductStorePrices(productId: Long): Future[Seq[ProductStorePrice]] = Future {
    val sql = "SELECT * FROM inv_product_stores WHERE product_id = @productId"

    val query = DB.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
      ProductStorePrice(rs.getLong("store_id"), DB.getNullable[BigDecimal]("price", rs))
    } _

    db.withConnection(query)
  }

  def getAttribute(attributeId: Long, lang: String): Future[Option[Attribute]] = Future {
    val sql =
      s"""
        SELECT
         	a.*,
         	COALESCE(t.label, dt.label) AS label,
         	COALESCE(t.short_description, dt.short_description) AS short_description,
         	COALESCE(t.long_description, dt.long_description) AS long_description,
          data_types.code AS data_type,
          input_types.code AS input_type
         FROM inv_attributes a
         	JOIN data_types ON data_types.id = a.data_type_id
         	JOIN input_types ON input_types.id = a.input_type_id
         	JOIN translations dt ON dt.description_id = a.description_id AND dt.is_default = 1
         	LEFT JOIN translations t ON t.description_id = a.description_id AND t.lang_id = @langId
         WHERE a.id = @attributeId
       """

    val query = DB.fetchOne(sql, Map(
      "langId" -> Lang.fromString(lang, 1).toString,
      "attributeId" -> attributeId.toString
    ))(Hydrators.hydrateAttribute) _

    db.withConnection(query)
  }

  def getProductAttributes(productId: Long, lang: String): Future[Set[ProductAttribute]] = Future {
    val sql =
      s"""
        SELECT
         	pa.id AS record_id, pa.attribute_value, pa.is_reference, pa.is_editable,
         	a.id AS attribute_id, a.code AS attribute_code, a.creation_date AS attribute_creation_date, a.modification_date AS attribute_modification_date,
         	data_types.code AS attribute_data_type, input_types.code AS attribute_input_type,
         	COALESCE(t.label, dt.label) AS attribute_name,
         	COALESCE(t.short_description, dt.short_description) AS attribute_short_description,
         	COALESCE(t.long_description, dt.long_description) AS attribute_long_description,
         	COALESCE(COALESCE(tv.label, dtv.label), pa.attribute_value) AS value,
          v.sku AS value_sku, v.id AS value_id
         FROM inv_product_attributes AS pa
         	JOIN inv_attributes a ON a.id = pa.attribute_id
         	JOIN data_types ON data_types.id = a.data_type_id
         	JOIN input_types ON input_types.id = a.input_type_id
         	JOIN translations dt ON dt.description_id = a.description_id AND dt.is_default = 1
         	LEFT JOIN translations t ON t.description_id = a.description_id AND t.lang_id = @langId
         	LEFT JOIN inv_values v ON v.id = pa.attribute_value AND pa.is_reference = 1
         	LEFT JOIN translations tv ON tv.description_id = v.description_id AND tv.lang_id = @langId
         	LEFT JOIN translations dtv ON dtv.description_id = v.description_id AND dtv.is_default = 1
         WHERE pa.product_id = @productId
         ORDER BY pa.id
       """

    val query = DB.fetchMany[ProductAttribute](sql, Map(
      "langId" -> Lang.fromString(lang, 1).toString,
      "productId" -> productId.toString
    ))(Hydrators.hydrateProductAttribute) _

    db.withConnection(query).to[ListSet]
  }

  def getProductAttribute(productId: Long, attributeId: Long, lang: String): Future[Option[ProductAttribute]] = Future {
    val sql =
      s"""
        SELECT
         	pa.id AS record_id, pa.attribute_value, pa.is_reference, pa.is_editable,
         	a.id AS attribute_id, a.code AS attribute_code, a.creation_date AS attribute_creation_date, a.modification_date AS attribute_modification_date,
         	data_types.code AS attribute_data_type, input_types.code AS attribute_input_type,
         	COALESCE(t.label, dt.label) AS attribute_name,
         	COALESCE(t.short_description, dt.short_description) AS attribute_short_description,
         	COALESCE(t.long_description, dt.long_description) AS attribute_long_description,
         	COALESCE(COALESCE(tv.label, dtv.label), pa.attribute_value) AS value,
          v.sku AS value_sku, v.id AS value_id
         FROM inv_product_attributes AS pa
         	JOIN inv_attributes a ON a.id = pa.attribute_id
         	JOIN data_types ON data_types.id = a.data_type_id
         	JOIN input_types ON input_types.id = a.input_type_id
         	JOIN translations dt ON dt.description_id = a.description_id AND dt.is_default = 1
         	LEFT JOIN translations t ON t.description_id = a.description_id AND t.lang_id = @langId
         	LEFT JOIN inv_values v ON v.id = pa.attribute_value AND pa.is_reference = 1
         	LEFT JOIN translations tv ON tv.description_id = v.description_id AND tv.lang_id = @langId
         	LEFT JOIN translations dtv ON dtv.description_id = v.description_id AND dtv.is_default = 1
         WHERE pa.product_id = @productId AND pa.attribute_id = @attributeId
       """

    val query = DB.fetchOne(sql, Map(
      "langId" -> Lang.fromString(lang, 1).toString,
      "productId" -> productId.toString,
      "attributeId" -> attributeId.toString,
    ))(Hydrators.hydrateProductAttribute) _

    db.withConnection(query)
  }

  def getProductDepartment(departmentId: Long, lang: String): Future[Option[ProductDepartment]] = Future {
    val sql =
      """
        SELECT
          d.*,
          COALESCE(t.label, dt.label) AS `d.name`,
          COALESCE(t.short_description, dt.short_description) AS `d.short_description`,
          COALESCE(t.long_description, dt.long_description) AS `d.long_description`
        FROM inv_departments d
        JOIN translations dt ON dt.description_id = d.description_id AND dt.is_default = 1
        LEFT JOIN translations t ON t.description_id = d.description_id AND t.lang_id = @langId
        WHERE d.id = @departmentId
      """
    val params = Map(
      "departmentId" -> departmentId.toString,
      "langId" -> Lang.fromString(lang, 1).toString
    )

    val query = DB.fetchOne(sql, params)(Hydrators.hydrateProductDepartment) _

    db.withConnection(query)
  }

  def getProductCategory(id: Long, lang: String): Future[Option[ProductCategory]] = {
    val step1 = Future {
      val sql =
        """
         SELECT
           c.*,
          COALESCE(t.label, dt.label) AS `c.name`,
          COALESCE(t.short_description, dt.short_description) AS `c.short_description`,
          COALESCE(t.long_description, dt.long_description) AS `c.long_description`
         FROM inv_product_categories AS c
         JOIN translations dt ON dt.description_id = c.description_id AND dt.is_default = 1
         LEFT JOIN translations t ON t.description_id = c.description_id AND t.lang_id = @langId
         WHERE c.id = @categoryId
       """
      val params = Map(
        "langId" -> Lang.fromString(lang, 1).toString,
        "categoryId" -> id.toString
      )

      val query = DB.fetchOne(sql, params)(Hydrators.hydrateProductCategory) _

      db.withConnection(query)
    }

    val step2 = for {
      pc <- OptionT(step1)
      parents <- OptionT.liftF(getCategoryParents(pc.id))
    } yield pc.copy(parents = parents)

    step2.value
  }

  def getProductCategories(lang: String): Future[Seq[(ProductCategory, Int)]] = Future {
    db.withConnection { conn =>
      val sql =
        """
        SELECT
          c.*,
          COALESCE(t.label, dt.label) AS `c.name`,
          COALESCE(t.short_description, dt.short_description) AS `c.short_description`,
          COALESCE(t.long_description, dt.long_description) AS `c.long_description`,
          IF(c.left_id = 1, 1, COUNT(parent.id)) AS depth
        FROM inv_product_categories c
          JOIN inv_product_categories parent ON c.left_id BETWEEN parent.left_id AND parent.right_id
          JOIN translations dt ON dt.description_id = c.description_id AND dt.is_default = 1
          LEFT JOIN translations t ON t.description_id = c.description_id AND t.lang_id = @langId
        GROUP BY c.id
        ORDER BY c.left_id
      """
      val params = Map(
        "langId" -> Lang.fromString(lang, 1).toString
      )

      val categories = DB.fetchMany(sql, params)(rs => {
        val category = Hydrators.hydrateProductCategory(rs)
        (category, rs.getInt("depth"))
      })(conn)

      categories
    }
  }

  def getAttributes(lang: String): Future[Seq[Attribute]] = Future {
    val sql =
      s"""
        SELECT
         	a.*,
         	COALESCE(t.label, dt.label) AS label,
         	COALESCE(t.short_description, dt.short_description) AS short_description,
         	COALESCE(t.long_description, dt.long_description) AS long_description,
          data_types.code AS data_type,
          input_types.code AS input_type
         FROM inv_attributes a
         	JOIN data_types ON data_types.id = a.data_type_id
         	JOIN input_types ON input_types.id = a.input_type_id
         	JOIN translations dt ON dt.description_id = a.description_id AND dt.is_default = 1
         	LEFT JOIN translations t ON t.description_id = a.description_id AND t.lang_id = @langId
         ORDER BY label ASC
       """

    val query = DB.fetchMany(sql, Map(
      "langId" -> Lang.fromString(lang, 1).toString,
    ))(Hydrators.hydrateAttribute) _

    db.withConnection(query)
  }

  def getProductDepartments(lang: String): Future[Seq[ProductDepartment]] = Future {
    val sql =
      """
        SELECT
          d.*,
          COALESCE(t.label, dt.label) AS `d.name`,
          COALESCE(t.short_description, dt.short_description) AS `d.short_description`,
          COALESCE(t.long_description, dt.long_description) AS `d.long_description`
        FROM inv_departments d
        JOIN translations dt ON dt.description_id = d.description_id AND dt.is_default = 1
        LEFT JOIN translations t ON t.description_id = d.description_id AND t.lang_id = @langId
      """
    val params = Map(
      "langId" -> Lang.fromString(lang, 1).toString
    )

    val query = DB.fetchMany(sql, params)(Hydrators.hydrateProductDepartment) _

    db.withConnection(query)
  }

  private def getProductChildren(productId: Long, lang: String): Future[Seq[ProductChild]] = {
    val step1 = Future {
      val sql = "SELECT id, sub_product_id, quantity, type, is_compiled, is_visible FROM inv_product_compositions WHERE product_id = @productId"
      val query = DB.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
        (
          rs.getLong("sub_product_id"),
          rs.getLong("id"),
          rs.getString("type"),
          rs.getLong("quantity"),
          rs.getBoolean("is_visible"),
          rs.getBoolean("is_compiled")
        )
      } _

      db.withConnection(query)
    }

    val step2 = step1.flatMap { records =>
      Future.traverse(records) { rec =>
        (for {
          product <- OptionT(getById(rec._1, lang))
        } yield ProductChild(rec._2, product, rec._3, rec._4, rec._5, rec._6)).value
      }
    }

    step2.map(_.flatten)
  }

  private def getCategoryParents(categoryId: Long): Future[SortedSet[String]] = Future {
    val sql = "SELECT DISTINCT parent.code FROM inv_product_categories actual JOIN inv_product_categories parent ON parent.left_id < actual.left_id AND parent.right_id > actual.right_id WHERE actual.id = @categoryId ORDER BY parent.left_id DESC"
    val query = DB.fetchMany(sql, Map("categoryId" -> categoryId.toString))(_.getString("code")) _

    db.withConnection(query).to[SortedSet]
  }

  def handleInclusions(product: Product, lang: String, include: Seq[String]): Future[Product] = {
    val futures = include.map(s => s match {
      case ProductInclusions.ATTRIBUTES =>
        getProductAttributes(product.id, lang).map { data =>
          (s, product.copy(attributes = data))
        }
      case ProductInclusions.CHILDREN =>
        getProductChildren(product.id, lang).map { data =>
          (s, product.copy(children = data))
        }
      case ProductInclusions.RULES =>
        getProductRules(product.id, lang).map { data =>
          (s, product.copy(rules = data))
        }
      case ProductInclusions.ASSEMBLY_PARTS =>
        getProductAssemblyParts(product.id, lang).map { data =>
          (s, product.copy(assemblyParts = data))
        }
      case _ => Future.successful((s, product))
    })

    Future.sequence(futures).map { tuples =>
      tuples.foldLeft(product) {
        (p, tuple) =>
          tuple match {
            case (ProductInclusions.ATTRIBUTES, other) => p.copy(attributes = other.attributes)
            case (ProductInclusions.CHILDREN, other) => p.copy(children = other.children)
            case (ProductInclusions.RULES, other) => p.copy(rules = other.rules)
            case (ProductInclusions.ASSEMBLY_PARTS, other) => p.copy(assemblyParts = other.assemblyParts)
            case _ => p
          }
      }
    }
  }

  def applyProductAttributeOverrides(product: Product, attributeOverrides: Seq[(String, String)], lang: String): Future[Product] = {
    val overridenAttributes: Seq[(ProductAttribute, String)] = attributeOverrides.flatMap(tuple => product.getAttribute(tuple._1) match {
      case Some(productAttribute) => Some(productAttribute, tuple._2)
      case _ => None
    })

    val productWithOverridenAttributes = overridenAttributes.foldLeft(Future.successful(product)) {
      (productF, tuple) =>
        (tuple._1, tuple._2, tuple._1.attribute.inputType) match {
          case (productAttribute, newValue, "select") =>
            productF.flatMap { p =>
              // Get the corresponding value
              (for {
                av <- OptionT(getAttributeValue(newValue.toLong, lang))
              } yield p.replaceAttribute(productAttribute, productAttribute.copy(
                value = av.description.name,
                valueId = Some(av.id),
                valueSku = Some(av.sku)
              ))).getOrElse(p)
            }
          case (productAttribute, newValue, _) => productF.map(p => p.replaceAttribute(productAttribute, productAttribute.copy(value = newValue)))
        }
    }

    productWithOverridenAttributes
  }
}
