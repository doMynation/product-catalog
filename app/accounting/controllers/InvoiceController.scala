package inventory.controllers

import javax.inject._

import accounting.repositories.InvoiceRepository
import inventory.actions.AuthenticatedAction
import inventory.requestAttributes.Attrs
import inventory.util.{CsvSerializer, SearchRequest}
import play.api.db.Database
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.ExecutionContext

class InvoiceController @Inject()(authAction: AuthenticatedAction, cc: ControllerComponents, db: Database, invoiceRepository: InvoiceRepository)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def get(id: Long) = Action { implicit req =>
    val csvExtractor = Accepting("text/csv")

    invoiceRepository.get(id).map { invoice =>
      render {
        case Accepts.Html() => Ok("HTML VERSION")
        case Accepts.Json() => Ok(Json.toJson(invoice))
        case csvExtractor() => Ok("CSV VERSION")
      }
    } getOrElse {
      BadRequest(s"Invoice ${id} not found")
    }
  }

  def search(include: Option[String]) = Action.async { implicit req =>
    val sr = SearchRequest.fromQueryString(req.queryString)
    val inclusions: Seq[String] = include.map(_.split(",").toSeq).getOrElse(Seq())

    invoiceRepository.search(sr, inclusions).map { invoices =>
      Ok(invoices.map(CsvSerializer.toCsv(_)).mkString("\r\n"))
    }
  }
}
