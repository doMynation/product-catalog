package inventory.controllers

import javax.inject.Inject

import inventory.ProductService
import inventory.dtos.{AttributeIdValuePair, ProductDepartmentDTO}
import inventory.entities.Admin.ProductEditData
import inventory.entities.{Product, ProductDepartment}
import inventory.forms.EditProductForm
import inventory.repositories.{ProductInclusions, ProductRepository, ProductWriteRepository}
import inventory.util.FileUploader
import inventory.validators.{DepartmentNotFound, DomainError, GenericError, InvalidPayload}
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.concurrent.{ExecutionContext, Future}
import cats.data.{EitherT, OptionT}
import cats.implicits._
import infrastructure.actions.SessionAction
import infrastructure.responses.{ApiError, ApiResponse}
import shared.Types.ServiceResponse

/**
  * Controller for the admin backend of the product catalog.
  */
class AdminProductController @Inject()(
                                        cc: ControllerComponents,
                                        productReadRepo: ProductRepository,
                                        productWriteRepo: ProductWriteRepository,
                                        productService: ProductService,
                                        uploader: FileUploader,
                                        authAction: SessionAction
                                      )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def createDepartment = authAction.async(parse.json) { req =>
    val json = req.body

    val program = for {
      dto <- EitherT.fromOption[Future](json.asOpt[ProductDepartmentDTO], InvalidPayload) // Parse JSON
      _ <- EitherT.fromEither[Future](dto.validate) // Validate payload
      deptId <- EitherT.rightT[Future, DomainError](productWriteRepo.createDepartment(dto)) // Create dept
      dept <- EitherT.fromOptionF[Future, DomainError, ProductDepartment](productReadRepo.getProductDepartment(deptId, "en"), DepartmentNotFound(deptId)) // Fetch the new product
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

  def get(productId: Long, lang: Option[String]) = authAction.async {
    val chosenLang = lang.getOrElse("en")
    val includes = List(ProductInclusions.ATTRIBUTES, ProductInclusions.CATEGORY, ProductInclusions.CHILDREN, ProductInclusions.RULES)

    // Get the product and its translations
    val program = for {
      product <- OptionT(productReadRepo.getById(productId, chosenLang, includes))
      translations <- OptionT.liftF(productReadRepo.getTranslations(product.descriptionId))
    } yield ProductEditData(product, translations)

    program
      .map(productData =>
        Ok(ApiResponse(Json.obj(
          "product" -> productData.product,
          "translations" -> productData.translations
        )).toJson)
      )
      .getOrElse(NotFound(s"Product id $productId not found"))
  }

  def delete(productId: Long) = authAction {
    productWriteRepo.deleteProduct(productId)

    Ok
  }

  def enable(productId: Long) = authAction {
    productWriteRepo.updateProductFields(productId, Map("status" -> "1"))

    Ok
  }

  def disable(productId: Long) = authAction {
    productWriteRepo.updateProductFields(productId, Map("status" -> "0"))

    Ok
  }

  def duplicate(productId: Long) = authAction.async {
    val response: ServiceResponse[Product] = productService.cloneProduct(productId)

    EitherT(response)
      .map(product => Ok(ApiResponse(product).toJson))
      .valueOr(err => BadRequest(ApiError.fromDomainError(err).toJson))
  }

  def update(productId: Long) = authAction(parse.json).async { req =>
    val data = (req.body \ "fields").validate[EditProductForm]

    val t = for {
      form <- EitherT.fromOption[Future](data.asOpt, BadRequest("Invalid parameters"))
      _ <- EitherT(productService.updateProduct(productId, form)).leftMap(err => BadRequest(ApiError(err.code, err.errorMessage).toJson))
    } yield Ok(ApiResponse.empty.toJson)

    t.valueOr(err => err)
  }

  def bulkEnable() = authAction(parse.json) { req =>
    val json = req.body
    val po = (json \ "productIds").asOpt[Seq[Long]]

    po.map { productIds =>
      productWriteRepo.bulkUpdateProduct(productIds, Map("status" -> "1"))
      Ok
    } getOrElse {
      BadRequest("Invalid `productIds` parameter")
    }
  }

  def bulkDisable() = authAction(parse.json) { req =>
    val json = req.body
    val po = (json \ "productIds").asOpt[Seq[Long]]

    po.map { productIds =>
      productWriteRepo.bulkUpdateProduct(productIds, Map("status" -> "0"))
      Ok
    } getOrElse {
      BadRequest("Invalid `productIds` parameter")
    }
  }

  def bulkAddAttributes = authAction(parse.json).async { req =>
    val json = req.body
    val productIds = (json \ "productIds").asOpt[List[Int]]
    val attributes = (json \ "attributes").asOpt[List[AttributeIdValuePair]]
    val error: DomainError = GenericError

    val program = for {
      ids <- EitherT.fromOption[Future](productIds, error)
      pairs <- EitherT.fromOption[Future](attributes, error)
      _ <- ids.traverse(
        id => EitherT(productService.addProductAttributes(id, pairs))
      )
    } yield ()

    program
      .map(_ => Ok)
      .valueOr(err => BadRequest(ApiError.fromDomainError(err).toJson))
  }
}
