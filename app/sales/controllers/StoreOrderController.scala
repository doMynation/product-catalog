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
import sales.entities.Order
import sales.repositories.{CustomerRepository, OrderRepository}
import shared.repositories.CommentRepository
import shared._

class StoreOrderController @Inject()(
                                      cc: ControllerComponents,
                                      orderRepository: OrderRepository,
                                      customerRepository: CustomerRepository,
                                      commentRepository: CommentRepository,
                                      salesService: SalesService
                                    ) extends AbstractController(cc) {

  def get(orderId: Long, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")
    val program: OptionT[IO, Result] = for {
      order <- OptionT(orderRepository.getById(orderId))
      if order.storeId == storeId
      include <- OptionT.liftF(handleIncludes(order, chosenLang, includeSet))
    } yield Ok(JsObject(Seq(
      "order" -> Json.toJson(order),
      "includes" -> Json.toJson(include)
    )))

    program
      .getOrElse(NotFound(s"Order $orderId not found"))
      .unsafeToFuture
  }

  def getByUUID(uuid: String, storeId: Long, lang: Option[String], include: Option[String]) = Action.async {
    val includeSet: Set[String] = include.fold(Seq[String]())(_.split(",")).toSet
    val chosenLang = lang.getOrElse("en")
    val orderUuid = UUID.fromString(uuid)

    val program: OptionT[IO, Result] = for {
      order <- OptionT(orderRepository.getById(orderUuid))
      if order.storeId == storeId
      include <- OptionT.liftF(handleIncludes(order, chosenLang, includeSet))
    } yield Ok(JsObject(Seq(
      "order" -> Json.toJson(order),
      "includes" -> Json.toJson(include)
    )))

    program
      .getOrElse(NotFound(s"Order $uuid not found"))
      .unsafeToFuture
  }

  def getComments(orderId: Long, storeId: Long) = Action.async { req =>
    val queryString = req.queryString ++ Map(
      "type" -> Seq(commentRepository.TYPE_ORDER),
      "documentId" -> Seq(orderId.toString),
    )
    val sr: SearchRequest = SearchRequest.fromQueryString(queryString)

    commentRepository
      .searchIO(sr)
      .map(searchResult => Ok(Json.toJson(searchResult)))
      .unsafeToFuture
  }

  def search(storeId: Long) = Action.async { req =>
    val queryString = req.queryString ++ Map("storeId" -> Seq(storeId.toString))
    val sr = SearchRequest.fromQueryString(queryString)

    orderRepository
      .search(sr)
      .map(searchResult => Ok(Json.toJson(searchResult)))
      .unsafeToFuture
  }

  private def handleIncludes(order: Order, lang: String, include: Set[String]): IO[Map[String, Includable]] = {
    include.foldLeft(IO.pure(Map.empty[String, Includable])) {
      (acc, includeCode) =>
        includeCode match {
          case "customer" =>
            (for {
              m <- OptionT.liftF(acc)
              data <- OptionT(customerRepository.getById(order.customerId))
            } yield m + (includeCode -> data)).getOrElseF(acc)
          case "lineItems" =>
            // Get each line items, fetch their corresponding product and apply attribute overrides
            for {
              m <- acc
              items <- orderRepository.getLineItems(order.id)
              itemsWithProduct <- items.traverse(salesService.populateLineItem(_, lang))
            } yield m + (includeCode -> LineItems(itemsWithProduct))
          case _ => acc
          case "expeditionDetails" =>
            (for {
              m <- OptionT.liftF(acc)
              expeditionDetails <- OptionT(orderRepository.getExpeditionDetails(order.id))
            } yield m + (includeCode -> expeditionDetails)).getOrElseF(acc)
          case "comments" =>
            // Create a search request for comments of this specific order
            val sr = SearchRequest(
              filters = Map("type" -> commentRepository.TYPE_ORDER, "documentId" -> order.id.toString),
              sortField = Some("createdAt"),
              limit = Some(100)
            )

            for {
              m <- acc
              searchResult <- commentRepository.searchIO(sr)
            } yield m + (includeCode -> Comments(searchResult.results))
        }
    }
  }
}
