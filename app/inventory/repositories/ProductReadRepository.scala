package inventory.repositories

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

  def handleInclusions(product: Product, lang: String, include: Seq[String]): IO[Product] = {
    db.use(Queries.includeRelations(product, lang, include).transact(_))
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
