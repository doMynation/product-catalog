package sales.controllers

import java.util.UUID
import javax.inject._
import cats.data.OptionT
import cats.implicits._
import inventory.repositories.{ProductInclusions}
import inventory.util.SearchRequest
import play.api.Logger
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import sales.SalesService
import sales.entities.Quote
import sales.repositories.{CustomerRepository, QuoteRepository}
import shared._
import utils.QuoteId
import scala.concurrent.{ExecutionContext, Future}

class StoreQuoteController @Inject()(
                                      cc: ControllerComponents,
                                      quoteRepository: QuoteRepository,
                                      customerRepository: CustomerRepository,
                                      salesService: SalesService
                                    )(implicit ec: ExecutionContext) extends AbstractController(cc) {
  private val logger = Logger("application")

  def get(quoteId: Long, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")

    val data = for {
      quote <- OptionT(quoteRepository.get(QuoteId(quoteId))) // @todo: Restrict to current store
      includes <- OptionT.liftF(handleIncludes(quote, chosenLang, includeSet))
    } yield (quote, includes)

    data map { tuple =>
      Ok(JsObject(Seq(
        "quote" -> Json.toJson(tuple._1),
        "includes" -> Json.toJson(tuple._2)
      )))
    } getOrElse {
      NotFound(s"Quote $quoteId not found")
    }
  }

  def getByUUID(uuid: String, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")
    val quoteUUID = UUID.fromString(uuid)

    val data = for {
      quote <- OptionT(quoteRepository.get(quoteUUID)) // @todo Restrict to current store
      includes <- OptionT.liftF(handleIncludes(quote, chosenLang, includeSet))
    } yield (quote, includes)

    data map { tuple =>
      Ok(JsObject(Seq(
        "quote" -> Json.toJson(tuple._1),
        "includes" -> Json.toJson(tuple._2)
      )))
    } getOrElse {
      NotFound(s"Quote $uuid not found")
    }
  }

  def search(storeId: Long) = Action.async { implicit req =>
    val queryString = req.queryString ++ Map("storeId" -> Seq(storeId.toString))
    val sr = SearchRequest.fromQueryString(queryString)

    quoteRepository.search(sr, Seq()).map { searchResult =>
      Ok(Json.toJson(searchResult))
    } recover {
      case t: Throwable =>
        logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  private def handleIncludes(quote: Quote, lang: String, include: Set[String]): Future[Map[String, Includable]] = {
    val futures: Seq[Future[Option[(String, Includable)]]] = (include collect {
      case "customer" => customerRepository.get(quote.customerId).map(_.map(("customer", _)))
      case "taxes" => quoteRepository.getTaxes(QuoteId(quote.id)).map(taxes => Some(("taxes", taxes)))
      case "attachments" => quoteRepository.getAttachments(QuoteId(quote.id)).map(attachments => Some(("attachments", FilesCollection(attachments))))
      case "lineItems" =>
        // Get each line items, fetch their corresponding product and apply attribute overrides
        for {
          items <- quoteRepository.getLineItems(QuoteId(quote.id))
          itemsWithProduct <- items.toList.traverse(salesService.populateLineItem(_, lang, List(ProductInclusions.ATTRIBUTES)))
        } yield Some(("lineItems", LineItems(itemsWithProduct)))
    }).toSeq

    // Execute all futures concurrently, and create a map out of the result
    Future.sequence(futures).map(_.flatten.toMap)
  }
}
