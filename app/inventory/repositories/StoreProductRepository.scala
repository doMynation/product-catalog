package inventory.repositories

import javax.inject.Inject
import cats.data.OptionT
import cats.implicits._
import infrastructure.DatabaseExecutionContext
import inventory.entities.ProductRule
import inventory.util.{DatabaseHelper, SearchRequest, SearchResult}
import play.api.db.Database
import scala.concurrent.Future
import shared.Types.Product
import shared.entities.Lang
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer

final class StoreProductRepository @Inject()(db: Database, productRepo: ProductRepository)(implicit ec: DatabaseExecutionContext) {
  def getById(id: Long, lang: String, storeId: Long): Future[Option[Product]] =
    getProduct("id", id.toString, lang, storeId)

  def getById(id: Long, lang: String, storeId: Long, include: Seq[String] = Seq()): Future[Option[Product]] = {
    val program = for {
      product <- OptionT(getById(id, lang, storeId))
      product <- OptionT.liftF(productRepo.handleInclusions(product, lang, include))
    } yield product

    program.value
  }

  def getBySku(sku: String, lang: String, storeId: Long): Future[Option[Product]] =
    getProduct("sku", sku, lang, storeId)

  def getBySku(sku: String, lang: String, storeId: Long, include: Seq[String] = Seq()): Future[Option[Product]] = {
    val program = for {
      product <- OptionT(getBySku(sku, lang, storeId))
      product <- OptionT.liftF(productRepo.handleInclusions(product, lang, include))
    } yield product

    program.value
  }

  private def getProduct(idType: String, id: String, lang: String, storeId: Long): Future[Option[Product]] = Future {
    val whereClause: String = idType match {
      case "id" => "p.id = @id"
      case "sku" => "p.sku = @id"
      case _ => "p.id = @id"
    }

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
            WHERE $whereClause
         """

    val query = DatabaseHelper.fetchOne(sql, Map(
      "id" -> id,
      "langId" -> Lang.fromString(lang, 1).toString,
      "storeId" -> storeId.toString,
    ))(Hydrators.hydrateProduct) _

    db.withConnection(query)
  }

  def getProductRule(id: Long, lang: String, storeId: Long): Future[Option[ProductRule]] = {
    val step1 = Future {
      val sql = "SELECT * FROM inv_product_relations pr JOIN inv_product_stores ps ON ps.product_id = pr.related_product_id AND ps.store_id = @storeId WHERE pr.id = @ruleId"
      val query = DatabaseHelper.fetchOne(sql, Map("ruleId" -> id.toString, "storeId" -> storeId.toString))(Hydrators.productRuleExtractor) _

      db.withConnection(query)
    }

    val step2 = for {
      rec <- OptionT(step1)
      product <- OptionT(getById(rec._1, lang, storeId))
    } yield ProductRule(rec._2, product, rec._3, rec._4, rec._5, rec._6)

    step2.value
  }

  def getProductRules(productId: Long, lang: String, storeId: Long): Future[Seq[ProductRule]] = {
    // Fetch the records
    val step1 = Future {
      val sql = "SELECT pr.* FROM inv_product_relations pr JOIN inv_product_stores ps ON ps.product_id = pr.related_product_id AND ps.store_id = @storeId WHERE pr.product_id = @productId"
      val query = DatabaseHelper.fetchMany(sql, Map("productId" -> productId.toString, "storeId" -> storeId.toString))(Hydrators.productRuleExtractor) _

      db.withConnection(query)
    }

    val step2 = step1.flatMap { records =>
      Future.traverse(records) { rec =>
        (for {
          product <- OptionT(getById(rec._1, lang, storeId))
        } yield ProductRule(rec._2, product, rec._3, rec._4, rec._5, rec._6)).value
      }
    }

    step2.map(_.flatten)
  }

  def search(sr: SearchRequest, lang: String, storeId: Long, include: Seq[String] = Seq()): Future[SearchResult[Product]] = {
    val wheres = new ListBuffer[String]()
    val havings = new ListBuffer[String]()
    val joins = new ListBuffer[String]()
    var params: Map[String, String] = Map(
      "langId" -> Lang.fromString(lang, 1).toString
    )

    val priceColumn = "p.retail_price"

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
        val products = DatabaseHelper.fetchMany(fetchSql, params)(Hydrators.hydrateProduct)(conn)
        val totalCount = DatabaseHelper.fetchColumn[Int]("SELECT FOUND_ROWS()")(conn)

        (products, totalCount.get)
      }
    }

    // Handle includes for all products
    val step2 = step1.flatMap { tuple =>
      val productsWithInclude = Future.traverse(tuple._1)(productRepo.handleInclusions(_, lang, include))
      productsWithInclude.map((_, tuple._2))
    }

    // Return a SearchResult
    step2.map(result => SearchResult(result._1, result._2))
  }
}
