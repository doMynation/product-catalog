package infra.actions

import javax.inject.Inject

import accounting.repositories.StoreRepository
import cats.data.OptionT
import cats.implicits._
import infra.requests.ApiRequest
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class ApiAction @Inject()(val parser: BodyParsers.Default, storeRepo: StoreRepository)
                         (implicit val executionContext: ExecutionContext) extends ActionBuilder[ApiRequest, AnyContent] {

  override def invokeBlock[A](request: Request[A], block: ApiRequest[A] => Future[Result]) = {
    // Get the api key out of the headers and lookup the corresponding store
    val result = for {
      apiKey <- OptionT.fromOption[Future](request.headers.get("X-Api-Key"))
      store <- OptionT(storeRepo.get(apiKey))
    } yield store

    result
      .map(store => block(ApiRequest(store, request)))
      .getOrElse(Future.successful(Unauthorized("Invalid API key")))
      .flatten
  }
}
