package inventory.controllers

import javax.inject._

import inventory.actions.AuthenticatedAction
import inventory.entities.Store
import inventory.repositories.ProductRepository
import inventory.requestAttributes.Attrs
import inventory.util.{DatabaseHelper, SearchRequest}
import play.api.db.Database
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.ExecutionContext

class StoreProductController @Inject()(authAction: AuthenticatedAction, cc: ControllerComponents, db: Database, productRepository: ProductRepository)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def getMultiple(idsString: String, lang: Option[String], include: Option[String]) = authAction { req =>
    implicit val store: Option[Store] = req.attrs.get(Attrs.Store)
    val ids = idsString.split(",").map(_.toInt)
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    if (ids.length > 100) {
      BadRequest("Maximum of 100 products at once")
    } else {
      val products = ids.flatMap(productRepository.get(_, chosenLang, includeSeq))

      Ok(Json.toJson(products))
    }
  }

  def getAttributeValues(attributeId: Long, lang: Option[String]) = authAction { req =>
    val values = productRepository.getAttributeValues(attributeId, lang.getOrElse("en"))

    Ok(Json.toJson(values))
  }

  def get(id: Long, lang: Option[String], include: Option[String]) = authAction { req =>
    implicit val store: Option[Store] = req.attrs.get(Attrs.Store)
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    productRepository.get(id, chosenLang, includeSeq).map { product =>
      Ok(Json.toJson(product))
    } getOrElse {
      NotFound(s"Product $id not found")
    }
  }

  def getBySku(sku: String, lang: Option[String], include: Option[String]) = authAction { req =>
    implicit val store: Option[Store] = req.attrs.get(Attrs.Store)
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    productRepository.getBySku(sku, chosenLang, includeSeq).map { product =>
      Ok(Json.toJson(product))
    } getOrElse {
      NotFound(s"Product $sku not found")
    }
  }

  def getRule(id: Long, lang: Option[String]) = authAction { req =>
    implicit val store: Option[Store] = req.attrs.get(Attrs.Store)
    val chosenLang = lang.getOrElse("en")

    productRepository.getRule(id, chosenLang).map { rule =>
      Ok(Json.toJson(rule))
    } getOrElse {
      NotFound(s"Rule $id not found")
    }
  }

  def getRules(id: Long, lang: Option[String]) = authAction { req =>
    implicit val store: Option[Store] = req.attrs.get(Attrs.Store)
    val chosenLang = lang.getOrElse("en")
    val rules = productRepository.getProductRules(id, chosenLang)

    Ok(Json.toJson(rules))
  }

  def search(lang: Option[String], include: Option[String]) = authAction.async { req =>
    implicit val store: Option[Store] = req.attrs.get(Attrs.Store)
    val sr = SearchRequest.fromQueryString(req.queryString)
    val inc: Seq[String] = include.map(_.split(",").toSeq).getOrElse(Seq())
    val chosenLang = lang.getOrElse("en")

    productRepository.search(sr, chosenLang, inc).map { searchResult =>
      Ok(Json.toJson(searchResult))
    } recover {
      case t: Throwable => ServiceUnavailable("Unexpected error")
    }
  }
}
