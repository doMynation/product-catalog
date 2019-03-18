package accounting.controllers

import java.util.UUID
import javax.inject._
import accounting.entities.Invoice
import accounting.repositories.InvoiceRepository
import cats.data.OptionT
import cats.effect.IO
import cats.implicits._
import inventory.util.SearchRequest
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import sales.SalesService
import sales.repositories.CustomerRepository
import shared.{Includable, LineItems}

class StoreInvoiceController @Inject()(
                                        cc: ControllerComponents,
                                        invoiceRepo: InvoiceRepository,
                                        customerRepository: CustomerRepository,
                                        salesService: SalesService
                                      ) extends AbstractController(cc) {

  def get(invoiceId: Long, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")

    val data = for {
      invoice <- OptionT(invoiceRepo.getById(invoiceId))
      if invoice.storeId == storeId
      includes <- OptionT.liftF(handleIncludes(invoice, chosenLang, includeSet))
    } yield (invoice, includes)

    data.map { tuple =>
      Ok(JsObject(Seq(
        "invoice" -> Json.toJson(tuple._1),
        "includes" -> Json.toJson(tuple._2)
      )))
    }
      .getOrElse(NotFound(s"Invoice $invoiceId not found"))
      .unsafeToFuture
  }

  def getByUUID(uuid: String, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")
    val invoiceUUID = UUID.fromString(uuid)

    val data = for {
      invoice <- OptionT(invoiceRepo.getById(invoiceUUID))
      if invoice.storeId == storeId
      includes <- OptionT.liftF(handleIncludes(invoice, chosenLang, includeSet))
    } yield (invoice, includes)

    data.map { tuple =>
      Ok(JsObject(Seq(
        "invoice" -> Json.toJson(tuple._1),
        "includes" -> Json.toJson(tuple._2)
      )))
    }
      .getOrElse(NotFound(s"Invoice $uuid not found"))
      .unsafeToFuture
  }

  def balance(storeId: Long) = Action.async {
    invoiceRepo
      .getStoreBalance(storeId)
      .map(balance => Ok(Json.toJson(balance)))
      .unsafeToFuture
  }

  def search(storeId: Long) = Action.async { implicit req =>
    val queryString = req.queryString ++ Map("storeId" -> Seq(storeId.toString))
    val sr = SearchRequest.fromQueryString(queryString)

    invoiceRepo
      .search(sr)
      .map(searchResult => Ok(Json.toJson(searchResult)))
      .unsafeToFuture
  }

  private def handleIncludes(invoice: Invoice, lang: String, include: Set[String]): IO[Map[String, Includable]] = {
    include.foldLeft(IO.pure(Map.empty[String, Includable])) {
      (acc, includeCode) =>
        includeCode match {
          case "taxes" =>
            for {
              m <- acc
              data <- invoiceRepo.getInvoiceTaxes(invoice.id)
            } yield m + (includeCode -> data)
          case "customer" =>
            (for {
              m <- OptionT.liftF(acc)
              data <- OptionT(customerRepository.getById(invoice.customerId))
            } yield m + (includeCode -> data)).getOrElseF(acc)
          case "lineItems" =>
            // Get each line items, fetch their corresponding product and apply attribute overrides
            for {
              m <- acc
              items <- invoiceRepo.getInvoiceLineItems(invoice.id)
              itemsWithProduct <- items.traverse(salesService.populateLineItemIO(_, lang))
            } yield m + (includeCode -> LineItems(itemsWithProduct))
          case _ => acc
        }
    }
  }
}
