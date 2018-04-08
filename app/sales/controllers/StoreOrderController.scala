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
import play.api.libs.json.{JsObject, JsString, Json}
import play.api.mvc._
import sales.entities.Order
import sales.repositories.{CustomerRepository, OrderRepository}
import shared.{Includable, LineItem, LineItems, OrderId}

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
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")

    val data = for {
      order <- OptionT(orderRepository.get(OrderId(orderId)))
      if order.storeId == storeId
      include <- OptionT.liftF(handleIncludes(order, chosenLang, includeSet))
    } yield (order, include)

    data map { tuple =>
      Ok(JsObject(Seq(
        "order" -> Json.toJson(tuple._1),
        "includes" -> Json.toJson(tuple._2)
      )))
    } getOrElse {
      NotFound(s"Order $orderId not found")
    }
  }

  def getByUUID(uuid: String, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")
    val orderUUID = UUID.fromString(uuid)

    val data = for {
      order <- OptionT(orderRepository.get(orderUUID))
      if order.storeId == storeId
      include <- OptionT.liftF(handleIncludes(order, chosenLang, includeSet))
    } yield (order, include)

    data map { tuple =>
      Ok(JsObject(Seq(
        "order" -> Json.toJson(tuple._1),
        "includes" -> Json.toJson(tuple._2)
      )))
    } getOrElse {
      NotFound(s"Order $uuid not found")
    }
  }

  def search(storeId: Long) = Action.async { req =>
    val queryString = req.queryString ++ Map("storeId" -> Seq(storeId.toString))
    val sr = SearchRequest.fromQueryString(queryString)

    orderRepository.search(sr) map { searchResult =>
      Ok(Json.toJson(searchResult))
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  private def handleIncludes(order: Order, lang: String, include: Set[String]): Future[Map[String, Includable]] = {
    val futures = (include collect {
      case "customer" => customerRepository.get(order.customerId).map(_.map(("customer", _)))
      case "lineItems" =>
        // Get the line items
        val lineItemsF: Future[Seq[LineItem]] = orderRepository.getLineItems(OrderId(order.id)).map(_.map { li =>
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
      case "expeditionDetails" => orderRepository.getExpeditionDetails(OrderId(order.id)).map(_.map(("expeditionDetails", _)))
    }).toSeq

    Future.sequence(futures).map(_.flatten.toMap)
  }
}
