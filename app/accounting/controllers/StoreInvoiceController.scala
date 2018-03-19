package inventory.controllers

import java.util.UUID
import javax.inject._

import accounting.entities.{Invoice, InvoiceInclude}
import accounting.repositories.{CustomerRepository, InvoiceRepository}
import cats.data.OptionT
import cats.implicits._
import inventory.actions.AuthenticatedAction
import inventory.repositories.ProductRepository
import inventory.util.SearchRequest
import play.api.Logger
import play.api.db.Database
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import shared.InvoiceId

import scala.concurrent.{ExecutionContext, Future}

class StoreInvoiceController @Inject()(
                                        authAction: AuthenticatedAction,
                                        cc: ControllerComponents,
                                        db: Database,
                                        invoiceRepository: InvoiceRepository,
                                        customerRepository: CustomerRepository,
                                        productRepository: ProductRepository
                                      )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def getByUUID(uuid: String, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val chosenLang = lang.getOrElse("en")
    val includeSeq: Seq[String] = include.fold(Seq[String]())(_.split(","))
    val invoiceUUID = UUID.fromString(uuid)

    val data = for {
      invoice <- OptionT(invoiceRepository.get(invoiceUUID, storeId))
      includes <- OptionT.liftF(handleIncludes(invoice, chosenLang, includeSeq))
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

  def get(invoiceId: Long, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSeq: Seq[String] = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    val data = for {
      invoice <- OptionT(invoiceRepository.get(InvoiceId(invoiceId), storeId))
      includes <- OptionT.liftF(handleIncludes(invoice, chosenLang, includeSeq))
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

  def search(storeId: Long) = Action.async { implicit req =>
    val queryString = req.queryString ++ Map("storeId" -> Seq(storeId.toString))
    val sr = SearchRequest.fromQueryString(queryString)

    invoiceRepository.search(sr, Seq()).map { searchResult =>
      Ok(Json.toJson(searchResult))
    } recover {
      case t: Throwable => {
        Logger.info(t.toString)
        ServiceUnavailable("Unexpected error")
      }
    }
  }

  def handleIncludes(invoice: Invoice, lang: String, codes: Seq[String]): Future[InvoiceInclude] = {
    codes.foldLeft(Future(InvoiceInclude())) { (acc, code) =>
      code match {
        case "taxes" => for {
          inc <- acc
          invoiceTaxes <- invoiceRepository.getTaxes(InvoiceId(invoice.id))
        } yield inc.copy(taxes = Some(invoiceTaxes))
        case "customer" => for {
          inc <- acc
          customer <- customerRepository.get(invoice.customerId)
        } yield inc.copy(customer = customer)
        case "lineItems" => {
          // Get the line items
          for {
            inc <- acc
            lineItems <- invoiceRepository.getLineItems(InvoiceId(invoice.id))
          } yield {
            // Get all products
            val lineItemsWithProducts = lineItems.map { li =>
              li.copy(product = li.productId.flatMap(productId => productRepository.get(productId, lang, List("attributes"))))
            }

            // Get the product details of each line item
            inc.copy(lineItems = Some(lineItemsWithProducts))
          }
        }
        case _ => acc
      }
    }
  }
}
