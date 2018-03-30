package sales.controllers

import javax.inject._

import cats.data.OptionT
import cats.implicits._
import inventory.actions.AuthenticatedAction
import inventory.repositories.ProductRepository
import inventory.util.SearchRequest
import play.api.Logger
import play.api.db.Database
import play.api.libs.json.{JsObject, JsString, Json}
import play.api.mvc._
import sales.repositories.{CustomerRepository, OrderRepository}
import shared.OrderId

import scala.concurrent.{ExecutionContext, Future}

class StoreOrderController @Inject()(
                                      authAction: AuthenticatedAction,
                                      cc: ControllerComponents,
                                      db: Database,
                                      orderRepository: OrderRepository,
                                      customerRepository: CustomerRepository,
                                      productRepository: ProductRepository
                                    )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def get(orderId: Long, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSeq: Seq[String] = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")

    val data = for {
      order <- OptionT(orderRepository.get(OrderId(orderId)))
      if order.storeId == storeId
    } yield order

    data map { order =>
      Ok(JsObject(Seq(
        "order" -> Json.toJson(order),
        "includes" -> JsString("")
      )))
    } getOrElse {
      NotFound(s"Order $orderId not found")
    }
  }

  def search(storeId: Long) = Action.async { req =>
    val queryString = req.queryString ++ Map("storeId" -> Seq(storeId.toString))
    val sr = SearchRequest.fromQueryString(queryString)

    orderRepository.search(sr) map { searchResult =>
      Ok(Json.toJson(searchResult))
    } recover {
      case t: Throwable => {
        Logger.info(t.toString)
        ServiceUnavailable("Unexpected error")
      }
    }
  }
}
