package inventory.repositories

import java.sql.ResultSet
import java.time.LocalDateTime

import com.google.inject.{Inject, Singleton}
import inventory.entities._
import inventory.util.{DatabaseHelper, SearchRequest}
import play.api.Logger
import play.api.db.Database

import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

@Singleton
final class ProductRepository @Inject()(db: Database)(implicit ec: ExecutionContext) {
  // Type alias
  type Product = inventory.entities.Product

  def getLangId(langCode: String): Int = langCode match {
    case "fr" => 1
    case "en" => 2
    case "es" => 3
    case _ => 1
  }

  def get(id: Long, lang: String, include: Seq[String] = Seq())(implicit store: Option[Store] = None): Option[Product] = {
    val product = store.map(getProductByStore(id, lang, _)) getOrElse getProduct(id, lang)

    product.map(handleInclusions(_, lang, include))
  }

  def getBySku(sku: String, lang: String, include: Seq[String] = Seq())(implicit store: Option[Store] = None): Option[Product] = {
    val product = store.map(getProductByStore(sku, lang, _)) getOrElse getProduct(sku, lang)

    product.map(handleInclusions(_, lang, include))
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

  private def getRules(productId: Long, lang: String): Seq[ProductRule] = db.withConnection { conn =>
    val sql = "SELECT * FROM inv_product_relations WHERE product_id = @productId"

    DatabaseHelper.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
      val ruleProductId = rs.getLong("related_product_id")
      get(ruleProductId, lang).map(hydrateProductRule(_, rs)).getOrElse(throw new RuntimeException(s"$ruleProductId does not exist"))
    }(conn)
  }

  def getRule(id: Long, lang: String)(implicit store: Option[Store] = None): Option[ProductRule] = {
    store.map(getProductRuleByStore(id, lang, _)) getOrElse getProductRule(id, lang)
  }

  private def getProductRule(id: Long, lang: String): Option[ProductRule] = db.withConnection { conn =>
    val sql = "SELECT * FROM inv_product_relations pr WHERE pr.id = @ruleId"

    DatabaseHelper.fetchOne(sql, Map("ruleId" -> id.toString)) { rs =>
      val ruleProductId = rs.getLong("related_product_id")
      get(ruleProductId, lang).map(hydrateProductRule(_, rs)).getOrElse(throw new RuntimeException(s"Product ${ruleProductId} not found"))
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
      get(ruleProductId, lang)(Some(store)).map(hydrateProductRule(_, rs)).getOrElse(throw new RuntimeException(s"Product ${ruleProductId} not found"))
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
              COALESCE(t.label, dt.label) AS `p.name`,
              COALESCE(t.short_description, dt.short_description) AS `p.short_description`,
              COALESCE(t.long_description, dt.long_description) AS `p.long_description`,
              COALESCE(tc.label, dtc.label) AS `c.name`,
              COALESCE(tc.short_description, dtc.short_description) AS `c.short_description`,
              COALESCE(tc.long_description, dtc.long_description) AS `c.long_description`
            FROM inv_products p
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
            LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = @langId
            JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
            LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = @langId
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
              COALESCE(t.label, dt.label) AS `p.name`,
              COALESCE(t.short_description, dt.short_description) AS `p.short_description`,
              COALESCE(t.long_description, dt.long_description) AS `p.long_description`,
              COALESCE(tc.label, dtc.label) AS `c.name`,
              COALESCE(tc.short_description, dtc.short_description) AS `c.short_description`,
              COALESCE(tc.long_description, dtc.long_description) AS `c.long_description`
            FROM inv_products p
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
            LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = @langId
            JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
            LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = @langId
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
              COALESCE(ps.price, p.retail_price) AS price,
              COALESCE(t.label, dt.label) AS `p.name`,
              COALESCE(t.short_description, dt.short_description) AS `p.short_description`,
              COALESCE(t.long_description, dt.long_description) AS `p.long_description`,
              COALESCE(tc.label, dtc.label) AS `c.name`,
              COALESCE(tc.short_description, dtc.short_description) AS `c.short_description`,
              COALESCE(tc.long_description, dtc.long_description) AS `c.long_description`
            FROM inv_products p
            JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
            LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = @langId
            JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
            LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = @langId
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
              COALESCE(ps.price, p.retail_price) AS price,
              COALESCE(t.label, dt.label) AS `p.name`,
              COALESCE(t.short_description, dt.short_description) AS `p.short_description`,
              COALESCE(t.long_description, dt.long_description) AS `p.long_description`,
              COALESCE(tc.label, dtc.label) AS `c.name`,
              COALESCE(tc.short_description, dtc.short_description) AS `c.short_description`,
              COALESCE(tc.long_description, dtc.long_description) AS `c.long_description`
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

    DatabaseHelper.fetchOne(sql, Map("lang" -> lang, "descriptionId" -> id.toString)) { rs =>
      Description(
        rs.getString("label"),
        rs.getString("short_description"),
        rs.getString("long_description")
      )
    }(conn)
  }

  def search(sr: SearchRequest, lang: String, include: Seq[String] = Seq())(implicit store: Option[Store] = None): Future[Seq[Product]] = Future {
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

      sr.filters.get("nameSku").foreach(value => {
        havings += "(`p.name` LIKE @name OR sku LIKE @sku)"
        params = params + ("name" -> s"%$value%", "sku" -> s"%$value%")
      })

      // `category` filter (e.g. "model", "model,option")
      sr.filters.get("category").foreach(value => {
        val categories = value.split(",")
        val inClause = categories.foldLeft(Queue[String]()) {
          (acc, categoryName) => acc :+ s"'$categoryName'"
        }.mkString(",")

        joins += s"JOIN inv_product_categories tree ON tree.code IN ($inClause) AND tree.left_id < c.left_id AND tree.right_id > c.right_id"
      })

      store.foreach(s => {
        s.id.foreach(storeId => {
          joins += "JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId"
          params = params + ("storeId" -> storeId.toString)
          priceColumn = "IFNULL(ps.price, p.retail_price)"
        })
      })

      val allowedSortFields = Map(
        "sku" -> "sku",
        "name" -> "`p.name`",
        "shortDescription" -> "`p.short_description`",
        "longDescription" -> "p.long_description`",
      )

      val sortField = sr.sortField.flatMap(allowedSortFields.get(_)).getOrElse("sku")
      Logger.info(s"Sorting by [$sortField]")

      val sql =
        s"""
              SELECT
                p.*,
                $priceColumn AS price,
                c.*,
                COALESCE(t.label, dt.label) AS `p.name`,
                COALESCE(t.short_description, dt.short_description) AS `p.short_description`,
                COALESCE(t.long_description, dt.long_description) AS `p.long_description`,
                COALESCE(tc.label, dtc.label) AS `c.name`,
                COALESCE(tc.short_description, dtc.short_description) AS `c.short_description`,
                COALESCE(tc.long_description, dtc.long_description) AS `c.long_description`
              FROM inv_products p
              JOIN inv_product_categories c ON c.id = p.category_id
              JOIN translations dt ON dt.description_id = p.description_id AND dt.is_default = 1
              LEFT JOIN translations t ON t.description_id = p.description_id AND t.lang_id = @langId
              JOIN translations dtc ON dtc.description_id = c.description_id AND dtc.is_default = 1
              LEFT JOIN translations tc ON tc.description_id = c.description_id AND tc.lang_id = @langId
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
              ORDER BY $sortField ${sr.sortOrder}
              ${sr.limit.map(lim => s"LIMIT ${sr.offset}, $lim").getOrElse("LIMIT 100")}
        """

      val products = DatabaseHelper.fetchMany(sql, params)(hydrateProduct)(conn)
      products.map(handleInclusions(_, lang, include))
    })
  }

  private def getProductAttributes(productId: Long, lang: String): Seq[ProductAttribute] = db.withConnection { conn =>
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
    ))(hydrateProductAttribute)(conn)
  }

  private def hydrateProductChild(product: Product, rs: ResultSet): ProductChild =
    ProductChild(product, rs.getString("type"), rs.getLong("quantity"), rs.getBoolean("is_visible"), rs.getBoolean("is_compiled"))

  private def hydrateProductAttribute(rs: ResultSet): ProductAttribute = {
    ProductAttribute(
      id = Some(rs.getLong("record_id")),
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
    val ts = rs.getTimestamp("modification_date")
    val updatedAt = if (rs.wasNull()) None else Option(ts.toLocalDateTime)

    val tagsValue = rs.getString("tags")
    val tags: Seq[String] = if (rs.wasNull()) Seq() else tagsValue.split(",")

    Product(
      Some(rs.getLong("id")),
      rs.getString("sku"),
      Description(rs.getString("p.name"), rs.getString("p.short_description"), rs.getString("p.long_description")),
      rs.getDouble("price"),
      rs.getDouble("cost_price"),
      tags = tags,
      category = Some(hydrateProductCategory(rs)),
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = updatedAt,
      isCustom = rs.getBoolean("is_custom")
    )
  }

  private def hydrateProductCategory(rs: ResultSet): ProductCategory = {
    val ts = rs.getTimestamp("c.modification_date")
    val updatedAt = if (rs.wasNull()) None else Option(ts.toLocalDateTime)

    ProductCategory(
      Some(rs.getLong("c.id")),
      Description(
        rs.getString("c.name"),
        rs.getString("c.short_description"),
        rs.getString("c.long_description")
      ),
      createdAt = rs.getTimestamp("c.creation_date").toLocalDateTime,
      updatedAt = updatedAt
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

  private def handleInclusions(product: Product, lang: String, include: Seq[String]) = {
    include.foldLeft(product) { (p, include) =>
      include match {
        case ProductInclusions.ATTRIBUTES => p.copy(attributes = getProductAttributes(product.id.get, lang))
        case ProductInclusions.CHILDREN => p.copy(children = getProductChildren(product.id.get, lang))
        case ProductInclusions.RULES => p.copy(rules = getProductRules(product.id.get, lang))
        case _ => p
      }
    }
  }

  private def hydrateProductRule(product: Product, rs: ResultSet): ProductRule = {
    ProductRule(
      id = rs.getLong("id"),
      product = product,
      newPrice = rs.getDouble("price"),
      ruleType = rs.getString("type"),
      quantity = rs.getInt("quantity"),
      maxAllowedQuantity = rs.getInt("max_quantity")
    )
  }
}
