package sales.controllers

import java.util.UUID
import javax.inject._
import cats.data.OptionT
import cats.effect.IO
import cats.implicits._
import inventory.util.SearchRequest
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import sales.SalesService
import sales.entities.Quote
import sales.repositories.{CustomerRepository, QuoteRepository}
import shared._

class StoreQuoteController @Inject()(
                                      cc: ControllerComponents,
                                      quoteRepository: QuoteRepository,
                                      customerRepository: CustomerRepository,
                                      salesService: SalesService
                                    ) extends AbstractController(cc) {

  def get(quoteId: Long, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")

    val program: OptionT[IO, Result] = for {
      quote <- OptionT(quoteRepository.getById(quoteId))
      if quote.storeId == storeId
      includes <- OptionT.liftF(handleIncludes(quote, chosenLang, includeSet))
    } yield Ok(JsObject(Seq(
      "quote" -> Json.toJson(quote),
      "includes" -> Json.toJson(includes)
    )))

    program
      .getOrElse(NotFound(s"Quote $quoteId not found"))
      .unsafeToFuture
  }

  def getByUUID(uuid: String, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")

    val program: OptionT[IO, Result] = for {
      quote <- OptionT(quoteRepository.getById(UUID.fromString(uuid)))
      if quote.storeId == storeId
      includes <- OptionT.liftF(handleIncludes(quote, chosenLang, includeSet))
    } yield Ok(JsObject(Seq(
      "quote" -> Json.toJson(quote),
      "includes" -> Json.toJson(includes)
    )))

    program
      .getOrElse(NotFound(s"Quote $uuid not found"))
      .unsafeToFuture
  }

  def search(storeId: Long) = Action.async { implicit req =>
    val queryString = req.queryString ++ Map("storeId" -> Seq(storeId.toString))
    val sr = SearchRequest.fromQueryString(queryString)

    quoteRepository
      .search(sr)
      .map(searchResult => Ok(Json.toJson(searchResult)))
      .unsafeToFuture
  }

  private def handleIncludes(quote: Quote, lang: String, include: Set[String]): IO[Map[String, Includable]] = {
    include.foldLeft(IO.pure(Map.empty[String, Includable])) {
      (acc, includeCode) =>
        includeCode match {
          case "customer" =>
            (for {
              m <- OptionT.liftF(acc)
              data <- OptionT(customerRepository.getById(quote.customerId))
            } yield m + (includeCode -> data)).getOrElseF(acc)
          case "lineItems" =>
            // Get each line items, fetch their corresponding product and apply attribute overrides
            for {
              m <- acc
              items <- quoteRepository.getQuoteLineItems(quote.id)
              itemsWithProduct <- items.traverse(salesService.populateLineItem(_, lang))
            } yield m + (includeCode -> LineItems(itemsWithProduct))
          case "attachments" =>
            for {
              m <- acc
              data <- quoteRepository.getAttachments(quote.id)
            } yield m + (includeCode -> FilesCollection(data))
          case "taxes" =>
            for {
              m <- acc
              data <- quoteRepository.getQuoteTaxes(quote.id)
            } yield m + (includeCode -> data)
          case _ => acc
        }
    }
  }
}
