package inventory.controllers

import javax.inject._
import inventory.actions.AuthenticatedAction
import inventory.repositories.ProductRepository
import inventory.requestAttributes.Attrs
import inventory.util.SearchRequest
import play.api.db.Database
import play.api.libs.json.Json
import play.api.mvc._
import scala.concurrent.ExecutionContext

class ProductController @Inject()(authAction: AuthenticatedAction, cc: ControllerComponents, db: Database, productRepository: ProductRepository)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def get(id: Long, lang: Option[String], include: Option[String]) = authAction { req =>
    implicit val store = req.attrs.get(Attrs.Store)
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    productRepository.get(id, chosenLang, includeSeq).map { product =>
      Ok(Json.toJson(product))
    } getOrElse {
      BadRequest(s"Product ${id} not found")
    }
  }

  def getBySku(sku: String, lang: Option[String], include: Option[String]) = authAction { req =>
    implicit val store = req.attrs.get(Attrs.Store)
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    productRepository.getBySku(sku, chosenLang, includeSeq).map { product =>
      Ok(Json.toJson(product))
    } getOrElse {
      BadRequest(s"Product ${sku} not found")
    }
  }

  def search(lang: Option[String], include: Option[String]) = authAction.async { req =>
    implicit val store = req.attrs.get(Attrs.Store)
    val sr = SearchRequest.fromQueryString(req.queryString)
    val inc: Seq[String] = include.map(_.split(",").toSeq).getOrElse(Seq())
    val chosenLang = lang.getOrElse("en")

    productRepository.search(sr, chosenLang, inc).map(products =>
      Ok(Json.toJson(products))
    ) recover {
      case t: Throwable => {
        println(t)
        ServiceUnavailable("Unexpected error")
      }
    }
  }
}
