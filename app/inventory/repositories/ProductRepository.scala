package inventory.repositories

import java.sql.ResultSet
import java.time.LocalDateTime
import javax.inject.Inject
import infrastructure.DatabaseExecutionContext
import inventory.entities._
import inventory.util.{DatabaseHelper, SearchRequest, SearchResult}
import play.api.db.Database
import scala.collection.immutable.{Queue, SortedSet}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class ProductRepository @Inject()(db: Database)(implicit ec: DatabaseExecutionContext) {
  // Type alias
  type Product = inventory.entities.Product

  private def getLangId(langCode: String): Int = langCode match {
    case "fr" => 1
    case "en" => 2
    case "es" => 3
    case _ => 1
  }

  private def getLangCode(langId: Long): String = langId match {
    case 1 => "fr"
    case 2 => "en"
    case 3 => "es"
    case _ => "fr"
  }

  def get(id: Long, lang: String, include: Seq[String] = Seq())(implicit store: Option[Store] = None): Option[Product] = {
    val product = store.map(getProductByStore(id, lang, _)) getOrElse getProduct(id, lang)

    product.map(handleInclusions(_, lang, include))
  }

  def getBySku(sku: String, lang: String, include: Seq[String] = Seq())(implicit store: Option[Store] = None): Option[Product] = {
    val product = store.map(getProductByStore(sku, lang, _)) getOrElse getProduct(sku, lang)

    product.map(handleInclusions(_, lang, include))
  }

  def getAttributeValue(valueId: Long, lang: String): Option[AttributeValue] = db.withConnection { conn =>
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

    DatabaseHelper.fetchOne(sql, Map(
      "langId" -> getLangId(lang).toString,
      "valueId" -> valueId.toString
    ))(hydrateAttributeValue)(conn)
  }

  def getAttributeValues(attributeId: Long, lang: String): Seq[AttributeValue] = db.withConnection { conn =>
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

    DatabaseHelper.fetchMany(sql, Map(
      "langId" -> getLangId(lang).toString,
      "attributeId" -> attributeId.toString
    ))(hydrateAttributeValue)(conn)
  }

  def getProductRules(productId: Long, lang: String)(implicit store: Option[Store] = None): Seq[ProductRule] = {
    store map (getProductRulesByStore(productId, lang, _)) getOrElse getRules(productId, lang)
  }

  def getRule(id: Long, lang: String)(implicit store: Option[Store] = None): Option[ProductRule] = {
    store.map(getProductRuleByStore(id, lang, _)) getOrElse getProductRule(id, lang)
  }

  def getTranslations(descriptionId: Long): Seq[Translation] = db.withConnection { conn =>
    val sql = "SELECT * FROM translations WHERE description_id = @descriptionId"

    DatabaseHelper.fetchMany(sql, Map("descriptionId" -> descriptionId.toString))(hydrateTranslation)(conn)
  }

  private def getRules(productId: Long, lang: String): Seq[ProductRule] = db.withConnection { conn =>
    val sql = "SELECT * FROM inv_product_relations WHERE product_id = @productId"

    DatabaseHelper.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
      val ruleProductId = rs.getLong("related_product_id")
      get(ruleProductId, lang).map(hydrateProductRule(_, rs)).getOrElse(throw new RuntimeException(s"$ruleProductId does not exist"))
    }(conn)
  }

  private def getProductRule(id: Long, lang: String): Option[ProductRule] = db.withConnection { conn =>
    val sql = "SELECT * FROM inv_product_relations pr WHERE pr.id = @ruleId"

    DatabaseHelper.fetchOne(sql, Map("ruleId" -> id.toString)) { rs =>
      val ruleProductId = rs.getLong("related_product_id")
      get(ruleProductId, lang).map(hydrateProductRule(_, rs)).getOrElse(throw new RuntimeException(s"Product $ruleProductId not found"))
    }(conn)
  }

  private def getProductRuleByStore(id: Long, lang: String, store: Store) = db.withConnection { conn =>
    val sql = "SELECT * FROM inv_product_relations pr JOIN inv_product_stores ps ON ps.product_id = pr.related_product_id AND ps.store_id = @storeId WHERE pr.id = @ruleId"
    val params = Map(
      "storeId" -> store.id.get.toString,
      "ruleId" -> id.toString,
    )

    DatabaseHelper.fetchOne(sql, params) { rs =>
      val ruleProductId = rs.getLong("related_product_id")
      get(ruleProductId, lang)(Some(store)).map(hydrateProductRule(_, rs)).getOrElse(throw new RuntimeException(s"Product $ruleProductId not found"))
    }(conn)
  }

  private def getProductRulesByStore(productId: Long, lang: String, store: Store): Seq[ProductRule] = db.withConnection { conn =>
    val sql = "SELECT pr.* FROM inv_product_relations pr JOIN inv_product_stores ps ON ps.product_id = pr.related_product_id AND ps.store_id = @storeId WHERE pr.product_id = @productId"
    val params = Map(
      "storeId" -> store.id.get.toString,
      "productId" -> productId.toString,
    )

    DatabaseHelper.fetchMany(sql, params) { rs =>
      val ruleProductId = rs.getLong("related_product_id")
      get(ruleProductId, lang)(Some(store)).map(hydrateProductRule(_, rs)).getOrElse(throw new RuntimeException(s"$ruleProductId does not exist"))
    }(conn)
  }

  private def getProduct(id: Long, lang: String): Option[Product] = db.withConnection { conn =>
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
            WHERE p.id = @productId
         """

    DatabaseHelper.fetchOne(sql, Map(
      "productId" -> id.toString,
      "langId" -> getLangId(lang).toString
    ))(hydrateProduct)(conn)
  }

  private def getProduct(sku: String, lang: String): Option[Product] = db.withConnection { conn =>
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
            WHERE p.sku = @sku
         """

    DatabaseHelper.fetchOne(sql, Map(
      "sku" -> sku,
      "langId" -> getLangId(lang).toString
    ))(hydrateProduct)(conn)
  }

  private def getProductByStore(id: Long, lang: String, store: Store): Option[Product] = db.withConnection { conn =>
    if (store.id.isEmpty) return None

    val sql =
      s"""
            SELECT
              p.*,
              c.*,
              d.*,
              COALESCE(ps.price, p.retail_price) AS price,
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
            JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
            LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = @langId
            JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
            LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = @langId
            LEFT JOIN inv_departments d ON d.id = p.department_id
            LEFT JOIN translations dtd ON dtd.description_id = d.description_id AND dtd.is_default = 1
            LEFT JOIN translations td ON td.description_id = d.description_id AND td.lang_id = @langId
            WHERE p.id = @productId
       """

    DatabaseHelper.fetchOne(sql, Map(
      "productId" -> id.toString,
      "langId" -> getLangId(lang).toString,
      "storeId" -> store.id.get.toString
    ))(hydrateProduct)(conn)
  }

  private def getProductByStore(sku: String, lang: String, store: Store): Option[Product] = db.withConnection(conn => {
    if (store.id.isEmpty) return None

    val sql =
      s"""
            SELECT
              p.*,
              c.*,
              d.*,
              COALESCE(ps.price, p.retail_price) AS price,
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
            JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
            LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = @langId
            JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
            LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = @langId
            WHERE p.sku = @sku
       """

    DatabaseHelper.fetchOne(sql, Map(
      "storeId" -> store.id.get.toString,
      "langId" -> getLangId(lang).toString,
      "sku" -> sku
    ))(hydrateProduct)(conn)
  })

  def getDescription(id: Long, lang: String): Option[Description] = db.withConnection { conn =>
    val sql = "SELECT t.* FROM translations AS t JOIN languages l ON l.id = t.lang_id AND t.code = @lang WHERE description_id = @descriptionId"

    DatabaseHelper.fetchOne(sql, Map("lang" -> lang, "descriptionId" -> id.toString))(hydrateDescription)(conn)
  }

  def search(sr: SearchRequest, lang: String, include: Seq[String] = Seq())(implicit store: Option[Store] = None): Future[SearchResult[Product]] = Future {
    db.withConnection(conn => {
      val wheres = new ListBuffer[String]()
      val havings = new ListBuffer[String]()
      val joins = new ListBuffer[String]()
      var params: Map[String, String] = Map(
        "langId" -> getLangId(lang).toString
      )

      var priceColumn = "p.retail_price"

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

      // `category` filter (e.g. "model", "model,option")
      sr.filters.get("category").foreach(value => {
        val categories = value.split(",")
        val inClause = categories.foldLeft(Queue[String]()) {
          (acc, categoryName) => acc :+ s"'$categoryName'"
        }.mkString(",")

        joins += s"JOIN inv_product_categories tree ON tree.code IN ($inClause) AND tree.left_id <= c.left_id AND tree.right_id >= c.right_id"
      })

      // When a implicit store is defined, enforce it and disallow filtering by store
      if (store.isDefined) {
        store.get.id.foreach(storeId => {
          joins += "JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId"
          params = params + ("storeId" -> storeId.toString)
          priceColumn = "IFNULL(ps.price, p.retail_price)"
        })
      } else {
        sr.filters.get("storeId").foreach(value => {
          joins += "JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId"
          params = params + ("storeId" -> value.toString)
        })
      }

      val allowedSortFields = Map(
        "sku" -> "sku",
        "name" -> "`p.name`",
        "shortDescription" -> "`p.short_description`",
        "longDescription" -> "`p.long_description`",
        "isCustom" -> "p.is_custom",
      )

      val sortField = sr.sortField.flatMap(allowedSortFields.get).getOrElse("sku")

      val fetchSql =
        s"""
              SELECT
                SQL_CALC_FOUND_ROWS
                p.*,
                $priceColumn AS price,
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
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
              ORDER BY $sortField ${sr.sortOrder}
              ${sr.limit.map(lim => s"LIMIT ${sr.offset}, $lim").getOrElse("LIMIT 100")}
        """

      val products = DatabaseHelper.fetchMany(fetchSql, params)(hydrateProduct)(conn).map(handleInclusions(_, lang, include))
      val totalCount = DatabaseHelper.fetchColumn[Int]("SELECT FOUND_ROWS()")(conn)

      SearchResult(products, totalCount.get)
    })
  }

  def getProductStorePrices(productId: Long): Seq[ProductStorePrice] = db.withConnection { conn =>
    val sql = "SELECT * FROM inv_product_stores WHERE product_id = @productId"

    DatabaseHelper.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
      ProductStorePrice(rs.getLong("store_id"), DatabaseHelper.getNullable[BigDecimal]("price", rs))
    }(conn)
  }

  def getProductAssemblyParts(productId: Long, lang: String): Seq[ProductAssemblyPart] = db.withConnection { conn =>
    val sql = "SELECT * FROM inv_product_assemblies WHERE product_id = @productId"

    DatabaseHelper.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
      val assemblyPartId = rs.getLong("assembly_product_id")

      get(assemblyPartId, lang)
        .map(part => ProductAssemblyPart(part, rs.getString("tag"), rs.getBoolean("is_default")))
        .getOrElse(throw new RuntimeException(s"$assemblyPartId does not exist"))
    }(conn)
  }

  private def getProductAttributes(productId: Long, lang: String): Set[ProductAttribute] = db.withConnection { conn =>
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
         WHERE pa.product_id = @productId;
       """

    DatabaseHelper.fetchMany(sql, Map(
      "langId" -> getLangId(lang).toString,
      "productId" -> productId.toString
    ))(hydrateProductAttribute)(conn).toSet
  }

  private def hydrateProductChild(product: Product, rs: ResultSet): ProductChild =
    ProductChild(product, rs.getString("type"), rs.getLong("quantity"), rs.getBoolean("is_visible"), rs.getBoolean("is_compiled"))

  private def hydrateProductAttribute(rs: ResultSet): ProductAttribute = {
    ProductAttribute(
      id = rs.getLong("record_id"),
      attribute = Attribute(
        rs.getLong("attribute_id"),
        rs.getString("attribute_code"),
        rs.getString("attribute_data_type"),
        rs.getString("attribute_input_type"),
        Description(
          rs.getString("attribute_name"),
          rs.getString("attribute_short_description"),
          rs.getString("attribute_long_description")
        ),
        rs.getTimestamp("attribute_creation_date").toLocalDateTime,
        DatabaseHelper.getNullable[LocalDateTime]("attribute_modification_date", rs)
      ),
      value = rs.getString("value"),
      valueId = DatabaseHelper.getNullable[Long]("value_id", rs),
      valueSku = DatabaseHelper.getNullable[String]("value_sku", rs),
      isEditable = rs.getBoolean("is_editable"),
      isReference = rs.getBoolean("is_reference")
    )
  }

  private def hydrateAttributeValue(rs: ResultSet): AttributeValue = {
    val ts = rs.getTimestamp("modification_date")
    val updatedAt = if (rs.wasNull()) None else Option(ts.toLocalDateTime)

    AttributeValue(
      rs.getLong("id"),
      rs.getString("sku"),
      Description(
        rs.getString("name"),
        rs.getString("short_description"),
        rs.getString("long_description")
      ),
      rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt
    )
  }

  private def hydrateProduct(rs: ResultSet): Product = {
    val metadata = Map(
      "mpn" -> rs.getString("mpn"),
      "isKit" -> rs.getInt("is_kit").toString,
      "imageUrl" -> rs.getString("image_url"),
      "stickerId" -> rs.getString("sticker_template_id"),
      "extrusionId" -> rs.getString("extrusion_template_id"),
    )

    val department = DatabaseHelper.getNullable[String]("d.code", rs).map { _ =>
      hydrateProductDepartment(rs)
    }

    Product(
      Some(rs.getLong("id")),
      rs.getLong("category_id"),
      rs.getString("sku"),
      rs.getLong("p.description_id"),
      Description(rs.getString("p.name"), rs.getString("p.short_description"), rs.getString("p.long_description")),
      rs.getDouble("price"),
      rs.getDouble("cost_price"),
      tags = DatabaseHelper.getNullable[String]("tags", rs).fold(Seq[String]())(_.split(",")),
      category = Some(hydrateProductCategory(rs)),
      department = department,
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = DatabaseHelper.getNullable[LocalDateTime]("modification_date", rs),
      metadata = metadata,
      isCustom = rs.getBoolean("is_custom"),
      isEnabled = rs.getBoolean("status"),
    )
  }

  private def hydrateProductCategory(rs: ResultSet): ProductCategory = {
    ProductCategory(
      Some(rs.getLong("c.id")),
      rs.getString("c.code"),
      Description(
        rs.getString("c.name"),
        rs.getString("c.short_description"),
        rs.getString("c.long_description")
      ),
      createdAt = rs.getTimestamp("c.creation_date").toLocalDateTime,
      updatedAt = DatabaseHelper.getNullable[LocalDateTime]("c.modification_date", rs)
    )
  }

  private def hydrateProductDepartment(rs: ResultSet): ProductDepartment = {
    ProductDepartment(
      rs.getLong("d.id"),
      rs.getString("d.code"),
      Description(
        rs.getString("d.name"),
        rs.getString("d.short_description"),
        rs.getString("d.long_description")
      ),
      createdAt = rs.getTimestamp("d.creation_date").toLocalDateTime,
      updatedAt = DatabaseHelper.getNullable[LocalDateTime]("d.modification_date", rs)
    )
  }

  private def getProductChildren(productId: Long, lang: String): Seq[ProductChild] = db.withConnection { conn =>
    val sql = "SELECT sub_product_id, quantity, type, is_compiled, is_visible FROM inv_product_compositions WHERE product_id = @productId"

    DatabaseHelper.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
      val childProductId = rs.getLong("sub_product_id")

      get(childProductId, lang)
        .map(product => hydrateProductChild(product, rs))
        .getOrElse(throw new RuntimeException(s"$childProductId does not exist"))
    }(conn)
  }

  def getProductCategory(id: Long, lang: String): Option[ProductCategory] = db.withConnection { conn =>
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
      "langId" -> getLangId(lang).toString,
      "categoryId" -> id.toString
    )

    val optCategory = DatabaseHelper.fetchOne(sql, params)(hydrateProductCategory)(conn)

    optCategory.map { category =>
      val parents = getCategoryParents(category.id.get)

      category.copy(parents = parents)
    }
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
          COUNT(parent.id) AS depth
        FROM inv_product_categories c
          JOIN inv_product_categories parent ON c.left_id BETWEEN parent.left_id AND parent.right_id
          JOIN translations dt ON dt.description_id = c.description_id AND dt.is_default = 1
          LEFT JOIN translations t ON t.description_id = c.description_id AND t.lang_id = @langId
        GROUP BY c.id
        ORDER BY c.left_id
      """
      val params = Map(
        "langId" -> getLangId(lang).toString
      )

      val categories = DatabaseHelper.fetchMany(sql, params)(rs => {
        val category = hydrateProductCategory(rs)
        (category, rs.getInt("depth"))
      })(conn)

      categories
    }
  }

  def getProductDepartments(lang: String): Future[Seq[ProductDepartment]] = Future {
    db.withConnection { conn =>
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
        "langId" -> getLangId(lang).toString
      )

      DatabaseHelper.fetchMany(sql, params)(hydrateProductDepartment)(conn);
    }
  }

  private def getCategoryParents(categoryId: Long): SortedSet[String] = db.withConnection { conn =>
    val sql = "SELECT DISTINCT parent.code FROM inv_product_categories actual JOIN inv_product_categories parent ON parent.left_id < actual.left_id AND parent.right_id > actual.right_id WHERE actual.id = @categoryId ORDER BY parent.left_id DESC"
    val parents = DatabaseHelper.fetchMany(sql, Map("categoryId" -> categoryId.toString))(_.getString("code"))(conn)

    parents.to[SortedSet]
  }

  private def handleInclusions(product: Product, lang: String, include: Seq[String]) = {
    include.foldLeft(product) { (p, include) =>
      include match {
        case ProductInclusions.ATTRIBUTES => p.copy(attributes = getProductAttributes(product.id.get, lang))
        case ProductInclusions.CHILDREN => p.copy(children = getProductChildren(product.id.get, lang))
        case ProductInclusions.RULES => p.copy(rules = getProductRules(product.id.get, lang))
        case ProductInclusions.ASSEMBLY_PARTS => p.copy(assemblyParts = getProductAssemblyParts(product.id.get, lang))
        case _ => p
      }
    }
  }

  private def hydrateProductRule(product: Product, rs: ResultSet): ProductRule =
    ProductRule(
      id = rs.getLong("id"),
      product = product,
      newPrice = rs.getDouble("price"),
      ruleType = rs.getString("type"),
      quantity = rs.getInt("quantity"),
      maxAllowedQuantity = rs.getInt("max_quantity")
    )

  private def hydrateTranslation(rs: ResultSet): Translation =
    Translation(
      id = rs.getLong("id"),
      lang = getLangCode(rs.getLong("lang_id")),
      description = hydrateDescription(rs),
      isDefault = rs.getBoolean("is_default")
    )

  private def hydrateDescription(rs: ResultSet): Description =
    Description(
      rs.getString("label"),
      rs.getString("short_description"),
      rs.getString("long_description")
    )

  def applyProductAttributeOverrides(product: Product, attributeOverrides: Seq[(String, String)], lang: String): Product = {
    val overridenAttributes: Seq[(ProductAttribute, String)] = attributeOverrides.flatMap(tuple => product.getAttribute(tuple._1) match {
      case Some(productAttribute) => Some(productAttribute, tuple._2)
      case _ => None
    })

    val productWithOverridenAttributes = overridenAttributes.foldLeft(product) {
      (product, tuple) =>
        (tuple._1, tuple._2, tuple._1.attribute.inputType) match {
          case (productAttribute, newValue, "select") =>
            // Get the corresponding value
            getAttributeValue(newValue.toLong, lang).map { attributeValue =>
              product.replaceAttribute(productAttribute, productAttribute.copy(
                value = attributeValue.description.name,
                valueId = Some(attributeValue.id),
                valueSku = Some(attributeValue.sku)
              ))
            } getOrElse product
          case (productAttribute, newValue, _) => product.replaceAttribute(productAttribute, productAttribute.copy(value = newValue))
        }
    }

    productWithOverridenAttributes
  }
}
