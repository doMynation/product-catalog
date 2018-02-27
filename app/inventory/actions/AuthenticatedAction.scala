package inventory.actions

import play.api.mvc._
import javax.inject.Inject

import accounting.repositories.StoreRepository
import inventory.entities.Store
import inventory.requestAttributes.Attrs
import play.api.Logger
import play.api.db.Database
import scala.concurrent.{ExecutionContext, Future}
import play.api.mvc.Results._

class AuthenticatedAction @Inject()(parser: BodyParsers.Default, db: Database, storeRepo: StoreRepository)
                                   (implicit ec: ExecutionContext) extends ActionBuilderImpl(parser) {

  private var cachedKeys = Map[(String, String), Option[Store]]()

  override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
    // Get the api key out of the headers and lookup the corresponding store
    val storeOpt = for {
      apiKey <- request.headers.get("X-Api-Key")
      store <- checkApiKey(apiKey, request.remoteAddress)
    } yield store

    // Save the store in the request and respond
    storeOpt.map { store =>
      block(request.addAttr(Attrs.Store, store))
    } getOrElse {
      Logger.info(s"Denied ${request.remoteAddress} with key ${request.headers.get("X-Api-Key").getOrElse("UNDEFINED")}")

      Future(Unauthorized("Invalid API Key"))
    }
  }

  private def checkApiKey(apiKey: String, remoteAddress: String): Option[Store] = {
    // First check against the cache
    if (cachedKeys.contains((apiKey, remoteAddress))) {
      return cachedKeys((apiKey, remoteAddress))
    }

    // Check if a store corresponds to the given api key and remote address
    val storeOpt = storeRepo.getByApiKey(apiKey, remoteAddress)

    // Cache the result
    cachedKeys = cachedKeys + ((apiKey, remoteAddress) -> storeOpt)

    storeOpt
  }
}
