package inventory.repositories

import java.sql.ResultSet
import com.google.inject.{Inject, Singleton}
import inventory.entities._
import inventory.util.{DatabaseHelper, SearchRequest}
import play.api.db.Database
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

@Singleton
final class ProductRepository @Inject()(db: Database)(implicit ec: ExecutionContext) {
  // Type alias
  type Product = inventory.entities.Product

  def get(id: Long, lang: String, include: Seq[String] = Seq())(implicit store: Option[Store] = None): Option[Product] = {
    val product = store.map(getProductByStore(id, lang, _)) getOrElse getProduct(id, lang)

    product.map(handleInclusions(_, lang, include))
  }

  def getBySku(sku: String, lang: String, include: Seq[String] = Seq())(implicit store: Option[Store] = None): Option[Product] = {
    val product = store.map(getProductByStore(sku, lang, _)) getOrElse getProduct(sku, lang)

    product.map(handleInclusions(_, lang, include))
  }

  private def getProduct(id: Long, lang: String): Option[Product] = db.withConnection { conn =>
    val sql =
      s"""
            SELECT p.*, p.retail_price AS price, c.*, t.*, tc.*
            FROM inv_products p
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations t ON t.description_id = p.description_id
            JOIN translations tc ON tc.description_id = c.description_id
            JOIN languages l ON l.id = t.lang_id AND l.code = @lang
            JOIN languages lc ON lc.id = tc.lang_id AND lc.code = @lang
            WHERE p.id = @productId
         """

    DatabaseHelper.fetchOne(sql, Map(
      "productId" -> id.toString,
      "lang" -> lang
    ))(hydrateProduct)(conn)
  }

  private def getProduct(sku: String, lang: String): Option[Product] = db.withConnection { conn =>
    val sql =
      s"""
            SELECT p.*, p.retail_price AS price, c.*, t.*, tc.*
            FROM inv_products p
            JOIN inv_product_categories c ON c.id = p.category_id
            JOIN translations t ON t.description_id = p.description_id
            JOIN translations tc ON tc.description_id = c.description_id
            JOIN languages l ON l.id = t.lang_id AND l.code = @lang
            JOIN languages lc ON lc.id = tc.lang_id AND lc.code = @lang
            WHERE p.sku = @sku
         """

    DatabaseHelper.fetchOne(sql, Map(
      "sku" -> sku,
      "lang" -> lang
    ))(hydrateProduct)(conn)
  }

  private def getProductByStore(id: Long, lang: String, store: Store): Option[Product] = db.withConnection { conn =>
    if (!store.id.isDefined) return None

    val sql =
      s"""
          SELECT p.*, c.*, t.*, tc.*, IFNULL(ps.price, p.retail_price) AS price
          FROM inv_products p
          JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = ?
          JOIN inv_product_categories c ON c.id = p.category_id
          JOIN translations t ON t.description_id = p.description_id
          JOIN translations tc ON tc.description_id = c.description_id
          JOIN languages l ON l.id = t.lang_id AND l.code = ?
          JOIN languages lc ON lc.id = tc.lang_id AND lc.code = ?
          WHERE p.id = ?
       """

    DatabaseHelper.fetchOne(sql, Map(
      "productId" -> id.toString,
      "lang" -> lang,
      "storeId" -> store.id.get.toString
    ))(hydrateProduct)(conn)
  }

  private def getProductByStore(sku: String, lang: String, store: Store): Option[Product] = db.withConnection(conn => {
    if (!store.id.isDefined) return None

    val sql =
      s"""
          SELECT p.*, c.*, t.*, tc.*, IFNULL(ps.price, p.retail_price) AS price
          FROM inv_products p
          JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId
          JOIN inv_product_categories c ON c.id = p.category_id
          JOIN translations t ON t.description_id = p.description_id
          JOIN translations tc ON tc.description_id = c.description_id
          JOIN languages l ON l.id = t.lang_id AND l.code = @lang
          JOIN languages lc ON lc.id = tc.lang_id AND lc.code = @lang
          WHERE p.sku = @sku
       """

    DatabaseHelper.fetchOne(sql, Map(
      "storeId" -> store.id.get.toString,
      "lang" -> lang,
      "sku" -> sku
    ))(hydrateProduct)(conn)
  })

  def getDescription(id: Long, lang: String): Option[Description] = db.withConnection { conn =>
    val sql = " SELECT t.* FROM translations AS t JOIN languages l ON l.id = t.lang_id AND t.code = @lang WHERE description_id = @descriptionId"

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
        "lang" -> lang
      )

      var priceColumn = "p.retail_price"

      wheres += "1 = 1"
      havings += "1 = 1"

      // `sku` filter
      sr.filters.get("sku").map(value => {
        wheres += "sku LIKE @sku"
        params = params + ("sku" -> s"%${value}%")
      })

      // `name` filter
      sr.filters.get("name").map(value => {
        wheres += "t.label LIKE @name"
        params = params + ("name" -> s"%${value}%")
      })

      // `label` filter (alias for `name`)
      sr.filters.get("label").map(value => {
        wheres += "t.label LIKE @name"
        params = params + ("name" -> s"%${value}%")
      })

      sr.filters.get("nameSku").map(value => {
        wheres += "(t.label LIKE @name OR sku LIKE @sku)"
        params = params + ("name" -> s"%${value}%", "sku" -> s"%${value}%")
      })

      // `category` filter (e.g. "model", "model,option")
      sr.filters.get("category").map(value => {
        val categories = value.split(",")
        val inClause = categories.foldLeft(Queue[String]()) {
          (acc, categoryName) => acc :+ s"'${categoryName}'"
        }.mkString(",")

        joins += s"JOIN inv_product_categories tree ON tree.code IN (${inClause}) AND tree.left_id < c.left_id AND tree.right_id > c.right_id";
      })

      store.map(s => {
        s.id.map(storeId => {
          joins += "JOIN inv_product_stores ps ON ps.product_id = p.id AND ps.store_id = @storeId"
          params = params + ("storeId" -> storeId.toString)
          priceColumn = "IFNULL(ps.price, p.retail_price)"
        })
      })

      val allowedSortFields = Set("sku", "name", "short_description", "long_description")
      val sortField = sr.sortField.filter(allowedSortFields).getOrElse("sku")

      val sql =
        s"""
              SELECT
                p.*,
                ${priceColumn} AS price,
                c.*,
                t.*,
                tc.*
              FROM inv_products p
              JOIN inv_product_categories c ON c.id = p.category_id
              JOIN translations t ON t.description_id = p.description_id
              JOIN translations tc ON tc.description_id = c.description_id
              JOIN languages lp ON lp.id = t.lang_id AND lp.code = @lang
              JOIN languages lc ON lc.id = tc.lang_id AND lc.code = @lang
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
              ORDER BY ${sortField} ${sr.sortOrder}
              ${sr.limit.map(lim => s"LIMIT ${sr.offset}, ${lim}").getOrElse("LIMIT 100")}
        """

      val products = DatabaseHelper.fetchMany(sql, params)(hydrateProduct)(conn)
      products.map(handleInclusions(_, lang, include))
    })
  }

  private def hydrateProductChild(product: Product, rs: ResultSet): ProductChild =
    ProductChild(product, rs.getString("type"), rs.getLong("quantity"), rs.getBoolean("is_visible"), rs.getBoolean("is_compiled"))

  private def hydrateProductAttribute(rs: ResultSet): ProductAttribute = {
    val ts = rs.getTimestamp("attribute_modification_date")
    val updatedAt = if (rs.wasNull()) None else Option(ts.toLocalDateTime)

    ProductAttribute(
      Some(rs.getLong("record_id")),
      Attribute(
        rs.getLong("attribute_id"),
        Description(
          rs.getString("attribute_name"),
          rs.getString("attribute_short_description"),
          rs.getString("attribute_long_description")
        ),
        rs.getTimestamp("attribute_creation_date").toLocalDateTime,
        updatedAt
      ),
      rs.getString("value")
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
      Description(rs.getString("label"), rs.getString("short_description"), rs.getString("long_description")),
      rs.getDouble("price"),
      rs.getDouble("cost_price"),
      tags = tags,
      category = Some(hydrateProductCategory(rs)),
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = updatedAt
    )
  }

  private def hydrateProductCategory(rs: ResultSet): ProductCategory = {
    val ts = rs.getTimestamp("c.modification_date")
    val updatedAt = if (rs.wasNull()) None else Option(ts.toLocalDateTime)

    ProductCategory(
      Some(rs.getLong("c.id")),
      Description(
        rs.getString("tc.label"),
        rs.getString("tc.short_description"),
        rs.getString("tc.long_description")
      ),
      createdAt = rs.getTimestamp("c.creation_date").toLocalDateTime,
      updatedAt = updatedAt
    )
  }

  private def getProductAttributes(productId: Long, lang: String): Seq[ProductAttribute] = db.withConnection { conn =>
    val sql =
      s"""
         SELECT
         a.id AS attribute_id, a.code AS attribute_code, a.creation_date AS attribute_creation_date, a.modification_date AS attribute_modification_date,
         t.label AS attribute_name, t.short_description AS attribute_short_description, t.long_description AS attribute_long_description,
         pa.id AS record_id, pa.attribute_value AS value, pa.is_reference, pa.is_editable
         FROM inv_product_attributes AS pa
         JOIN inv_attributes a ON a.id = pa.attribute_id
         JOIN translations t ON t.description_id = a.description_id
         JOIN languages l ON l.id = t.lang_id AND l.code = @lang
         WHERE pa.product_id = @productId
       """

    DatabaseHelper.fetchMany(sql, Map(
      "lang" -> lang,
      "productId" -> productId.toString
    ))(hydrateProductAttribute)(conn)
  }

  private def getProductChildren(productId: Long, lang: String): Seq[ProductChild] = db.withConnection { conn =>
    val sql = "SELECT sub_product_id, quantity, type, is_compiled, is_visible FROM inv_product_compositions WHERE product_id = @productId"

    DatabaseHelper.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
      val childProductId = rs.getLong("sub_product_id")

      get(childProductId, lang)
        .map(product => hydrateProductChild(product, rs))
        .getOrElse(throw new RuntimeException(s"${childProductId} does not exist"))
    }(conn)
  }

  private def getProductRules(productId: Long, lang: String): Seq[ProductRule] = db.withConnection { conn =>
    val sql = "SELECT * FROM inv_product_relations WHERE product_id = @productId"

    DatabaseHelper.fetchMany(sql, Map("productId" -> productId.toString)) { rs =>
      val ruleProductId = rs.getLong("related_product_id")
      get(ruleProductId, lang).map(hydrateProductRule(_, rs)).getOrElse(throw new RuntimeException(s"${ruleProductId} does not exist"))
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
      product = product,
      newPrice = rs.getDouble("price"),
      ruleType = rs.getString("type"),
      quantity = rs.getInt("quantity"),
      maxAllowedQuantity = rs.getInt("max_quantity")
    )
  }
}
