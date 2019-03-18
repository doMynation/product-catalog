package inventory.repositories

import javax.inject.Inject
import cats.data.OptionT
import cats.implicits._
import infra.DatabaseExecutionContext
import inventory.entities.ProductRule
import inventory.util.{DB}
import play.api.db.Database
import scala.concurrent.Future
import shared.Types.Product
import shared.entities.Lang

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

    val query = DB.fetchOne(sql, Map(
      "id" -> id,
      "langId" -> Lang.fromString(lang, 1).toString,
      "storeId" -> storeId.toString,
    ))(Hydrators.hydrateProduct) _

    db.withConnection(query)
  }

  def getProductRule(id: Long, lang: String, storeId: Long): Future[Option[ProductRule]] = {
    val step1 = Future {
      val sql = "SELECT * FROM inv_product_relations pr JOIN inv_product_stores ps ON ps.product_id = pr.related_product_id AND ps.store_id = @storeId WHERE pr.id = @ruleId"
      val query = DB.fetchOne(sql, Map("ruleId" -> id.toString, "storeId" -> storeId.toString))(Hydrators.productRuleExtractor) _

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
      val query = DB.fetchMany(sql, Map("productId" -> productId.toString, "storeId" -> storeId.toString))(Hydrators.productRuleExtractor) _

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
}
