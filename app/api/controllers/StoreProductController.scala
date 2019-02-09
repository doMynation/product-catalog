package api.controllers

import javax.inject._
import cats.data.OptionT
import cats.implicits._
import infra.actions.ApiAction
import infra.responses.ApiResponse
import inventory.repositories.{ProductRepository, StoreProductRepository}
import inventory.util.SearchRequest
import play.api.libs.json.Json
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}

class StoreProductController @Inject()(
                                        apiAction: ApiAction,
                                        cc: ControllerComponents,
                                        productRepo: ProductRepository,
                                        storeRepo: StoreProductRepository,
                                      )(implicit ec: ExecutionContext) extends AbstractController(cc) {


  def get(id: Long, lang: Option[String], include: Option[String]) = apiAction.async { req =>
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")
    val program = storeRepo.getById(id, chosenLang, req.store.id, includeSeq)

    OptionT(program)
      .map(product => Ok(Json.toJson(product)))
      .getOrElse(NotFound(s"Product $id not found"))
  }

  def getBySku(sku: String, lang: Option[String], include: Option[String]) = apiAction.async { req =>
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")
    val program = storeRepo.getBySku(sku, chosenLang, req.store.id, includeSeq)

    OptionT(program)
      .map(product => Ok(Json.toJson(product)))
      .getOrElse(NotFound(s"Product $sku not found"))
  }

  def getMultiple(idsString: String, lang: Option[String], include: Option[String]) = apiAction.async { req =>
    val ids = idsString.split(",").map(_.toInt).toList
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    if (ids.length > 100) {
      Future.successful(BadRequest("Maximum of 100 products at once"))
    } else {
      val result = ids.traverse(storeRepo.getById(_, chosenLang, req.store.id, includeSeq))

      result.map(products => Ok(Json.toJson(products.flatten)))
    }
  }

  def getAttributeValues(attributeId: Long, lang: Option[String]) = apiAction.async {
    productRepo
      .getAttributeValues(attributeId, lang.getOrElse("en"))
      .map(values => Ok(Json.toJson(values)))
  }

  def getRule(id: Long, lang: Option[String]) = apiAction.async { req =>
    val chosenLang = lang.getOrElse("en")
    val program = storeRepo.getProductRule(id, chosenLang, req.store.id)

    OptionT(program)
      .map(rule => Ok(Json.toJson(rule)))
      .getOrElse(NotFound(s"Rule $id not found"))
  }

  def getRules(id: Long, lang: Option[String]) = apiAction.async { req =>
    val chosenLang = lang.getOrElse("en")

    storeRepo
      .getProductRules(id, chosenLang, req.store.id)
      .map(rules => Ok(Json.toJson(rules)))
  }

  def search(lang: Option[String], include: Option[String]) = apiAction.async { req =>
    // Restrict products searched by store to enabled ones only
    val queryString = req.queryString ++ Map("IsEnabled" -> List("1"))
    val sr = SearchRequest.fromQueryString(queryString)
    val inc: Seq[String] = include.map(_.split(",").toSeq).getOrElse(Seq())
    val chosenLang = lang.getOrElse("en")

    storeRepo
      .search(sr, chosenLang, req.store.id, inc)
      .map { searchResult =>
        Ok(Json.toJson(
          ApiResponse(searchResult.results, Map(
            "count" -> searchResult.totalCount.toString,
            "query" -> req.rawQueryString
          ))
        ))
      }
  }
}
