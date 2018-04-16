package sales.controllers

import java.util.UUID
import javax.inject._
import cats.data.OptionT
import cats.implicits._
import inventory.actions.AuthenticatedAction
import inventory.repositories.{ProductInclusions, ProductRepository}
import inventory.util.SearchRequest
import play.api.Logger
import play.api.db.Database
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import sales.entities.Quote
import sales.repositories.{CustomerRepository, QuoteRepository}
import shared._
import scala.concurrent.{ExecutionContext, Future}

class StoreQuoteController @Inject()(
                                      authAction: AuthenticatedAction,
                                      cc: ControllerComponents,
                                      db: Database,
                                      quoteRepository: QuoteRepository,
                                      customerRepository: CustomerRepository,
                                      productRepository: ProductRepository
                                    )(implicit ec: ExecutionContext) extends AbstractController(cc) {

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
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  private def handleIncludes(quote: Quote, lang: String, include: Set[String]): Future[Map[String, Includable]] = {
    val futures: Seq[Future[Option[(String, Includable)]]] = (include collect {
      case "customer" => customerRepository.get(quote.customerId).map(_.map(("customer", _)))
      case "taxes" => quoteRepository.getTaxes(QuoteId(quote.id)).map(taxes => Some(("taxes", taxes)))
      case "attachments" => quoteRepository.getAttachments(QuoteId(quote.id)).map(attachments => Some(("attachments", FilesCollection(attachments))))
      case "lineItems" =>
        // Get the line items
        val lineItemsF: Future[Seq[LineItem]] = quoteRepository.getLineItems(QuoteId(quote.id)).map(_.map { li =>
          // Get each line item's corresponding product (if any)
          val overridenProduct = for {
            productId <- li.productId
            product <- productRepository.get(productId, lang, List(ProductInclusions.ATTRIBUTES))
          } yield productRepository.applyProductAttributeOverrides(product, li.attributeOverrides, lang)

          li.copy(product = overridenProduct)
        })

        lineItemsF.map(lineItems =>
          Some(("lineItems", LineItems(lineItems)))
        )
    }).toSeq

    // Execute all futures concurrently, and create a map out of the result
    Future.sequence(futures).map(_.flatten.toMap)
  }
}
