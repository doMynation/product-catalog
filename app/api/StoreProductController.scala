package api

import javax.inject._

import authentication.actions.ApiAction
import cats.data.OptionT
import cats.implicits._
import inventory.entities.Store
import inventory.repositories.StoreProductRepository
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.ExecutionContext

final class StoreProductController @Inject()(
                                              apiAction: ApiAction,
                                              cc: ControllerComponents,
                                              productRepo: StoreProductRepository
                                            )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def get(id: Long, lang: Option[String], include: Option[String]) = apiAction.async { req =>
    val includeSeq = include.fold(Seq[String]())(_.split(","))
    val chosenLang = lang.getOrElse("en")
    val result = OptionT(productRepo.getById(id, chosenLang, req.store.id, includeSeq))

    result
      .map(product => Ok(Json.toJson(product)))
      .getOrElse(NotFound(s"Product $id not found"))
  }
}
