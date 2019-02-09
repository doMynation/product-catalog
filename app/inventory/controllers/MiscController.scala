package inventory.controllers

import javax.inject.Inject

import infra.actions.ApiAction
import inventory.repositories.MiscRepository
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.concurrent.ExecutionContext

class MiscController @Inject()(
                                apiAction: ApiAction,
                                cc: ControllerComponents,
                                miscRepository: MiscRepository
                              )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def getExtrusions = apiAction.async {
    miscRepository.getExtrusions.map { extrusions =>
      Ok(Json.toJson(extrusions))
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }
}

