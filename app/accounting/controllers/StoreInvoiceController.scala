package inventory.controllers

import javax.inject._

import accounting.entities.{Invoice, InvoiceInclude, InvoiceTax, TaxComponent}
import accounting.repositories.InvoiceRepository
import akka.actor.ActorSystem
import cats.data.OptionT
import cats.implicits._
import inventory.actions.AuthenticatedAction
import inventory.util.SearchRequest
import play.api.Logger
import play.api.db.Database
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import shared.InvoiceId

import scala.concurrent.{ExecutionContext, Future}

class StoreInvoiceController @Inject()(authAction: AuthenticatedAction, cc: ControllerComponents, db: Database, invoiceRepository: InvoiceRepository, actorSystem: ActorSystem)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def get(invoiceId: Long, storeId: Long, include: Option[String]) = Action.async {
    val includeSeq: Seq[String] = include.fold(Seq[String]())(_.split(","))

    val data = for {
      invoice <- OptionT(invoiceRepository.get(InvoiceId(invoiceId), storeId))
      includes <- OptionT.liftF(handleIncludes(invoice, includeSeq))
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

  private def handleIncludes(invoice: Invoice, codes: Seq[String]): Future[InvoiceInclude] = {
    codes.foldLeft(Future(InvoiceInclude())) {
      (acc, code) =>
        code match {
          case "taxes" => for {
            inc <- acc
            invoiceTaxes <- invoiceRepository.getInvoiceTaxes(invoice.id)
          } yield inc.copy(taxes = invoiceTaxes)
          case _ => acc
        }
    }
  }
}
