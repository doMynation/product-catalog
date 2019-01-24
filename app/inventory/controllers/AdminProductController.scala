package inventory.controllers

import javax.inject.Inject
import authentication.actions.AuthenticatedAction
import infrastructure.{ApiError, ApiResponse}
import inventory.ProductService
import inventory.dtos.{AttributeIdValuePair, ProductDepartmentDTO}
import inventory.entities.Admin.ProductEditData
import inventory.entities.ProductDepartment
import inventory.forms.EditProductForm
import inventory.repositories.{ProductInclusions, ProductRepository, ProductWriteRepository}
import inventory.util.FileUploader
import inventory.validators.{DepartmentNotFound, DomainError, InvalidPayload}
import play.api.{Logger}
import play.api.db.Database
import play.api.libs.json.{JsError, JsSuccess, Json}
import play.api.mvc.{AbstractController, ControllerComponents}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import cats.data.EitherT
import cats.implicits._

/**
  * Controller for the admin backend of the product catalog.
  */
class AdminProductController @Inject()(
                                        cc: ControllerComponents,
                                        productReadRepository: ProductRepository,
                                        productWriteRepository: ProductWriteRepository,
                                        productService: ProductService,
                                        uploader: FileUploader,
                                        authAction: AuthenticatedAction
                                      )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def createDepartment = authAction.async(parse.json) { req =>
    val json = req.body

    val program: EitherT[Future, DomainError, ProductDepartment] = for {
      dto <- EitherT.fromEither[Future](json.asOpt[ProductDepartmentDTO].toRight(InvalidPayload))
      _ <- EitherT.fromEither[Future](dto.validate)
      deptId <- EitherT.right[DomainError](productWriteRepository.createDepartment(dto))
      dept <- EitherT.fromEither[Future](
        productReadRepository
          .getProductDepartment(deptId, "en")
          .toRight[DomainError](DepartmentNotFound(deptId))
      )
    } yield dept

    program
      .map(dept => Ok(ApiResponse(dept).toJson))
      .valueOr(error => BadRequest(ApiError(error.code, error.errorMessage).toJson))
  }

  def upload = authAction(parse.multipartFormData) { request =>
    request.body.file("filepond").map { file =>
      val tf = uploader.upload(file)

      // @todo: Eventually remove this.
      // For some reason, the file isn't immediately available after moving it to the upload directly.
      Thread.sleep(3000)

      Ok(tf.getFileName.toString)
    } getOrElse {
      BadRequest("Didn't work")
    }
  }

  def get(productId: Long, lang: Option[String]) = authAction {
    val chosenLang = lang.getOrElse("en")
    val includes = List(ProductInclusions.ATTRIBUTES, ProductInclusions.CATEGORY, ProductInclusions.CHILDREN, ProductInclusions.RULES)

    // Get the product and its translations
    val dataOpt = for {
      product <- productReadRepository.get(productId, chosenLang, includes)
      translations = productReadRepository.getTranslations(product.descriptionId)
    } yield ProductEditData(product, translations)

    dataOpt map { productData =>
      Ok(ApiResponse(Json.obj(
        "product" -> productData.product,
        "translations" -> productData.translations
      )).toJson)
    } getOrElse {
      NotFound(s"Product id $productId not found")
    }
  }

  def delete(productId: Long) = authAction.async {
    productWriteRepository
      .deleteProduct(productId)
      .map(_ => Ok)
      .recover {
        case t: Throwable =>
          Logger.error(t.toString)
          ServiceUnavailable("Unexpected error")
      }
  }

  def enable(productId: Long) = authAction.async {
    productWriteRepository
      .updateProductFields(productId, Map("status" -> "1"))
      .map(_ => Ok)
      .recover {
        case t: Throwable =>
          Logger.error(t.toString)
          ServiceUnavailable("Unexpected error")
      }
  }

  def disable(productId: Long) = authAction.async {
    productWriteRepository.updateProductFields(productId, Map("status" -> "0"))
      .map(_ => Ok)
      .recover {
        case t: Throwable =>
          Logger.error(t.toString)
          ServiceUnavailable("Unexpected error")
      }
  }

  def duplicate(productId: Long) = authAction {
    val productTry = productService.cloneProduct(productId)

    productTry match {
      case Success(product) => Ok(ApiResponse(product).toJson)
      case Failure(t) =>
        Logger.error(t.toString)
        ServiceUnavailable("Unexpected error")
    }
  }

  def update(productId: Long) = authAction(parse.json) { req =>
    val data = (req.body \ "fields").validate[EditProductForm]

    data match {
      case form: JsSuccess[EditProductForm] =>
        productService.updateProduct(productId, form.get) match {
          case Left(error) => BadRequest(ApiError(error.code, error.errorMessage).toJson)
          case _ => Ok(ApiResponse.empty.toJson)
        }
      case e: JsError =>
        println(e)
        BadRequest("Invalid parameters")
    }
  }

  def bulkEnable() = authAction.async(parse.json) { req =>
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

  def bulkDisable() = authAction.async(parse.json) { req =>
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

  def bulkAddAttributes = authAction(parse.json) { req =>
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
