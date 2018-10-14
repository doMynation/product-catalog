package inventory.controllers

import javax.inject.Inject

import inventory.ProductService
import inventory.dtos.AttributeIdValuePair
import inventory.entities.Admin.ProductEditData
import inventory.forms.EditProductForm
import inventory.repositories.{ProductInclusions, ProductRepository, ProductWriteRepository}
import play.api.Logger
import play.api.db.Database
import play.api.libs.json.{Json, Reads}
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * Controller for the admin backend of the product catalog.
  */
class AdminProductController @Inject()(
                                        cc: ControllerComponents,
                                        db: Database,
                                        productReadRepository: ProductRepository,
                                        productWriteRepository: ProductWriteRepository,
                                        productService: ProductService,
                                      )(implicit ec: ExecutionContext) extends AbstractController(cc) {
  def test = Action {
    Ok("HI")
  }

  def get(productId: Long, lang: Option[String]) = Action {
    val chosenLang = lang.getOrElse("en")
    val includes = List(ProductInclusions.ATTRIBUTES, ProductInclusions.CATEGORY, ProductInclusions.CHILDREN, ProductInclusions.RULES)

    // Get the product and its translations
    val dataOpt = for {
      product <- productReadRepository.get(productId, chosenLang, includes)
      translations = productReadRepository.getTranslations(product.descriptionId)
    } yield ProductEditData(product, translations)

    dataOpt map { productData =>
      Ok(Json.obj("product" -> productData.product, "translations" -> productData.translations))
    } getOrElse {
      NotFound(s"Product id $productId not found")
    }
  }

  def delete(productId: Long) = Action.async {
    productWriteRepository.deleteProduct(productId).map { _ =>
      Ok
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  def enable(productId: Long) = Action.async {
    val future = productWriteRepository.updateProductFields(productId, Map("status" -> "1"))

    future.map { _ =>
      Ok
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  def disable(productId: Long) = Action.async {
    val future = productWriteRepository.updateProductFields(productId, Map("status" -> "0"))

    future.map { _ =>
      Ok
    } recover {
      case t: Throwable =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  def duplicate(productId: Long) = Action {
    val productTry = productService.cloneProduct(productId)

    productTry match {
      case Success(product) => Ok(Json.toJson(product))
      case Failure(t) =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  def update(productId: Long) = Action(parse.json) { req =>
    val data = (req.body \ "fields").asOpt[EditProductForm]

    data map { form =>
      productService.updateProduct(productId, form)
      Ok
    } getOrElse {
      BadRequest("Invalid parameters")
    }
  }

  def bulkEnable() = Action.async(parse.json) { req =>
    val json = req.body
    val po = (json \ "productIds").asOpt[Seq[Long]]

    po.map { productIds =>
      productWriteRepository
        .bulkUpdateProduct(productIds, Map("status" -> "1"))
        .map(_ => Ok)
        .recover {
          case t: Throwable =>
            Logger.error(t.toString)
            ServiceUnavailable("Unexpected error")
        }
    } getOrElse {
      Future.successful(BadRequest("Invalid `productIds` parameter"))
    }
  }

  def bulkDisable() = Action.async(parse.json) { req =>
    val json = req.body
    val value = (json \ "productIds").asOpt[Seq[Long]]

    value.map { productIds =>
      productWriteRepository
        .bulkUpdateProduct(productIds, Map("status" -> "0"))
        .map(_ => Ok)
        .recover {
          case t: Throwable =>
            Logger.error(t.toString)
            ServiceUnavailable("Unexpected error")
        }
    } getOrElse {
      Future.successful(BadRequest("Invalid `productIds` parameter"))
    }
  }

  def bulkAddAttributes = Action(parse.json) { req =>
    val json = req.body
    val productIds = (json \ "productIds").asOpt[List[Int]]
    val attributes = (json \ "attributes").asOpt[List[AttributeIdValuePair]]

    val ot = for {
      ids <- productIds
      pairs <- attributes
    } yield Try {
      ids.foreach(id => productService.addProductAttributes(id, pairs))
    }

    ot match {
      case Some(Success(_)) => Ok
      case None => BadRequest("Invalid parameters")
      case Some(Failure(t)) =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }
}
