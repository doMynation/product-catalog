package inventory.controllers

import javax.inject._

import inventory.entities.AttributeValue
import inventory.repositories.ProductRepository
import inventory.util.SearchRequest
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import play.api.Logger
import cats.implicits._
import infrastructure.actions.ApiAction
import infrastructure.responses.ApiResponse

class ProductController @Inject()(
                                   apiAction: ApiAction,
                                   cc: ControllerComponents,
                                   productRepo: ProductRepository,
                                 )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def getAttributes(lang: Option[String]) = apiAction.async {
    val chosenLang = lang.getOrElse("en")

    for {
      attrs <- productRepo.getAttributes(chosenLang)
      attrsWithValues <- attrs.toList.traverse { attribute =>
        val valuesF =
          if (attribute.inputType == "select") productRepo.getAttributeValues(attribute.id, chosenLang)
          else Future.successful(List.empty[AttributeValue])

        valuesF.map(values => attribute.copy(values = values))
      }
    } yield Ok(Json.toJson(attrsWithValues))
  }

  def getDepartments(lang: Option[String]) = apiAction.async {
    val chosenLang = lang.getOrElse("en")

    productRepo.getProductDepartments(chosenLang).map { departments =>
      Ok(Json.toJson(departments))
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  def getCategories(lang: Option[String]) = apiAction.async {
    val chosenLang = lang.getOrElse("en")

    productRepo.getProductCategories(chosenLang).map { categories =>
      Ok(Json.toJson(categories))
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  def getMultiple(idsString: String, lang: Option[String], include: Option[String]) = apiAction.async {
    val ids = idsString.split(",").map(_.toInt).toList
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    if (ids.length > 100) {
      Future.successful(BadRequest("Maximum of 100 products at once"))
    } else {
      val products = ids.traverse(productRepo.getById(_, chosenLang, includeSeq))

      products.map(products => Ok(Json.toJson(products.flatten)))
    }
  }

  def get(id: Long, lang: Option[String], include: Option[String]) = apiAction.async {
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    productRepo
      .getById(id, chosenLang, includeSeq)
      .map {
        case Some(product) => Ok(Json.toJson(product))
        case _ => NotFound(s"Product $id not found")
      }
  }

  def getBySku(sku: String, lang: Option[String], include: Option[String]) = apiAction.async {
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    productRepo
      .getBySku(sku, chosenLang, includeSeq)
      .map {
        case Some(product) => Ok(Json.toJson(product))
        case _ => NotFound(s"Product $sku not found")
      }
  }

  def getRule(id: Long, lang: Option[String]) = apiAction.async {
    val chosenLang = lang.getOrElse("en")

    productRepo
      .getProductRule(id, chosenLang)
      .map {
        case Some(rule) => Ok(Json.toJson(rule))
        case _ => NotFound(s"Product $id not found")
      }
  }

  def getRules(id: Long, lang: Option[String]) = apiAction.async {
    val chosenLang = lang.getOrElse("en")

    productRepo
      .getProductRules(id, chosenLang)
      .map(rules => Ok(Json.toJson(rules)))
  }

  def getAttributeValues(attributeId: Long, lang: Option[String]) = apiAction.async {
    productRepo
      .getAttributeValues(attributeId, lang.getOrElse("en"))
      .map(values => Ok(Json.toJson(values)))
  }

  def search(lang: Option[String], include: Option[String]) = apiAction.async { req =>
    val sr = SearchRequest.fromQueryString(req.queryString)
    val inc: Seq[String] = include.map(_.split(",").toSeq).getOrElse(Seq())
    val chosenLang = lang.getOrElse("en")

    productRepo.search(sr, chosenLang, inc).map { searchResult =>
      Ok(
        ApiResponse(searchResult.results, Map(
          "count" -> searchResult.totalCount.toString,
          "query" -> req.rawQueryString
        )).toJson
      )
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  def getCategory(id: Long, lang: Option[String]) = apiAction.async {
    val chosenLang = lang.getOrElse("en")

    productRepo
      .getProductCategory(id, chosenLang)
      .map {
        case Some(cat) => Ok(Json.toJson(cat))
        case _ => NotFound(s"Product category $id not found")
      }
  }
}
