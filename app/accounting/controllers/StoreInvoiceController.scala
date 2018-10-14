package accounting.controllers

import java.util.UUID
import javax.inject._
import accounting.entities.Invoice
import accounting.repositories.InvoiceRepository
import cats.data.OptionT
import cats.implicits._
import inventory.actions.AuthenticatedAction
import inventory.repositories.ProductRepository
import inventory.util.SearchRequest
import play.api.Logger
import play.api.db.Database
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import sales.repositories.CustomerRepository
import shared.{Includable, InvoiceId, LineItem, LineItems}
import scala.concurrent.{ExecutionContext, Future}

class StoreInvoiceController @Inject()(
                                        authAction: AuthenticatedAction,
                                        cc: ControllerComponents,
                                        db: Database,
                                        invoiceRepository: InvoiceRepository,
                                        customerRepository: CustomerRepository,
                                        productRepository: ProductRepository
                                      )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def get(invoiceId: Long, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")

    val data = for {
      invoice <- OptionT(invoiceRepository.get(InvoiceId(invoiceId), storeId))
      includes <- OptionT.liftF(handleIncludes(invoice, chosenLang, includeSet))
    } yield (invoice, includes)

    data map { tuple =>
      Ok(JsObject(Seq(
        "invoice" -> Json.toJson(tuple._1),
        "includes" -> Json.toJson(tuple._2)
      )))
    } getOrElse {
      NotFound(s"Invoice $invoiceId not found")
    }
  }

  def getByUUID(uuid: String, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")
    val invoiceUUID = UUID.fromString(uuid)

    val data = for {
      invoice <- OptionT(invoiceRepository.get(invoiceUUID, storeId))
      includes <- OptionT.liftF(handleIncludes(invoice, chosenLang, includeSet))
    } yield (invoice, includes)

    data map { tuple =>
      Ok(JsObject(Seq(
        "invoice" -> Json.toJson(tuple._1),
        "includes" -> Json.toJson(tuple._2)
      )))
    } getOrElse {
      NotFound(s"Invoice $uuid not found")
    }
  }

  def balance(storeId: Long) = Action.async {
    val balanceT = OptionT(invoiceRepository.getStoreBalance(storeId))

    balanceT
      .map(balance => Ok(Json.toJson(balance)))
      .getOrElse(NotFound(s"Store $storeId not found"))
  }

  def search(storeId: Long) = Action.async { implicit req =>
    val queryString = req.queryString ++ Map("storeId" -> Seq(storeId.toString))
    val sr = SearchRequest.fromQueryString(queryString)

    invoiceRepository.search(sr, Seq()).map { searchResult =>
      Ok(Json.toJson(searchResult))
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  private def handleIncludes(invoice: Invoice, lang: String, include: Set[String]): Future[Map[String, Includable]] = {
    val futures: Seq[Future[Option[(String, Includable)]]] = (include collect {
      case "customer" => customerRepository.get(invoice.customerId).map(_.map(("customer", _)))
      case "taxes" => invoiceRepository.getTaxes(InvoiceId(invoice.id)).map(taxes => Some(("taxes", taxes)))
      case "lineItems" =>
        // Get the line items
        val lineItemsF: Future[Seq[LineItem]] = invoiceRepository.getLineItems(InvoiceId(invoice.id)).map(_.map { li =>
          // Get each line item's corresponding product (if any)
          li.copy(product = li.productId.flatMap(productRepository.get(_, lang)))
        })

        lineItemsF.map(lineItems =>
          Some(("lineItems", LineItems(lineItems)))
        )
    }).toSeq

    // Execute all futures concurrently, and create a map out of the result
    Future.sequence(futures).map(_.flatten.toMap)
  }
}
