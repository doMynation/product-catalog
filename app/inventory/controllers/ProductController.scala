package inventory.controllers

import cats.data.OptionT
import cats.effect.IO
import javax.inject._
import inventory.entities.AttributeValue
import inventory.repositories.{ProductReadRepository, ProductRepository}
import inventory.util.SearchRequest
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import play.api.Logger
import cats.implicits._
import infra.actions.ApiAction
import infra.responses.ApiResponse

class ProductController @Inject()(
                                   apiAction: ApiAction,
                                   cc: ControllerComponents,
                                   productRepo: ProductRepository,
                                   doobieRepo: ProductReadRepository,
                                 )(implicit ec: ExecutionContext) extends AbstractController(cc) {
  private val logger = Logger("application")

  def getAttributes(lang: Option[String]) = apiAction.async {
    val chosenLang = lang.getOrElse("en")
    val program = for {
      attrs <- doobieRepo.getAttributes(chosenLang)
      attrsWithValues <- attrs.traverse { attribute =>
        val valuesF =
          if (attribute.inputType == "select") doobieRepo.getAttributeValues(attribute.id, chosenLang)
          else IO.pure(List.empty[AttributeValue])

        valuesF.map(values => attribute.copy(values = values))
      }
    } yield Ok(Json.toJson(attrsWithValues))

    program.unsafeToFuture
  }

  def getDepartments(lang: Option[String]) = apiAction.async {
    doobieRepo
      .getProductDepartments(lang.getOrElse("en"))
      .map(depts => Ok(Json.toJson(depts)))
      .unsafeToFuture
  }

  def getCategories(lang: Option[String]) = apiAction.async {
    doobieRepo
      .getProductCategories(lang.getOrElse("en"))
      .map(categories => Ok(Json.toJson(categories)))
      .unsafeToFuture
  }

  def getMultiple(idsString: String, lang: Option[String], include: Option[String]) = apiAction.async {
    val ids = idsString.split(",").map(_.toInt).toList
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    val program: OptionT[IO, Result] = for {
      _ <- OptionT.fromOption[IO](if (ids.length <= 100) Some(ids) else None)
      products <- OptionT.liftF(ids.traverse(doobieRepo.getById(_, chosenLang, includeSeq)))
    } yield Ok(Json.toJson(products.flatten))

    program
      .getOrElse(BadRequest("Maximum of 100 products at once"))
      .unsafeToFuture
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
        logger.error(t.toString)
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
