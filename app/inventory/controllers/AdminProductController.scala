package inventory.controllers

import javax.inject.Inject
import inventory.ProductService
import inventory.dtos.{AttributeIdValuePair, ProductDepartmentDTO}
import inventory.entities.Admin.ProductEditData
import inventory.entities.{ProductDepartment}
import inventory.forms.EditProductForm
import inventory.repositories.{ProductInclusions, ProductReadRepository, ProductWriteRepository}
import inventory.util.FileUploader
import inventory.validators.{DepartmentNotFound, DomainError, GenericError, InvalidPayload}
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents, Result}
import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.implicits._
import infra.actions.SessionAction
import infra.responses.{ApiError, ApiResponse}

/**
  * Controller for the admin backend of the product catalog.
  */
class AdminProductController @Inject()(
                                        cc: ControllerComponents,
                                        productReadRepo: ProductReadRepository,
                                        productWriteRepo: ProductWriteRepository,
                                        productService: ProductService,
                                        uploader: FileUploader,
                                        authAction: SessionAction
                                      ) extends AbstractController(cc) {

  def createDepartment = authAction.async(parse.json) { req =>
    val json = req.body

    val program: EitherT[IO, DomainError, ProductDepartment] = for {
      dto <- EitherT.fromOption[IO](json.asOpt[ProductDepartmentDTO], InvalidPayload) // Parse JSON
      _ <- EitherT.fromEither[IO](dto.validate) // Validate payload
      deptId <- EitherT.right[DomainError](productWriteRepo.createDepartment(dto)) // Create dept
      dept <- EitherT.fromOptionF[IO, DomainError, ProductDepartment](productReadRepo.getProductDepartment(deptId, "en"), DepartmentNotFound(deptId)) // Fetch the new product
    } yield dept

    program
      .map(dept => Ok(ApiResponse(dept).toJson))
      .valueOr(error => BadRequest(ApiError(error.code, error.errorMessage).toJson))
      .unsafeToFuture
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
    val program: OptionT[IO, ProductEditData] = for {
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
      .unsafeToFuture
  }

  def delete(productId: Long) = authAction.async {
    productWriteRepo
      .deleteProduct(productId)
      .map(_ => Ok)
      .unsafeToFuture
  }

  def enable(productId: Long) = authAction.async {
    productWriteRepo
      .updateProductFields(productId, Map("status" -> "1"))
      .map(_ => Ok)
      .unsafeToFuture
  }

  def disable(productId: Long) = authAction.async {
    productWriteRepo
      .updateProductFields(productId, Map("status" -> "0"))
      .map(_ => Ok)
      .unsafeToFuture
  }

  def duplicate(productId: Long) = authAction.async {
    productService.cloneProduct(productId)
      .map(product => Ok(ApiResponse(product).toJson))
      .valueOr(err => BadRequest(ApiError.fromDomainError(err).toJson))
      .unsafeToFuture
  }

  def update(productId: Long) = authAction(parse.json).async { req =>
    val data: Option[EditProductForm] = (req.body \ "fields").asOpt[EditProductForm]
    val program: EitherT[IO, Result, Result] = for {
      form <- EitherT.fromOption[IO](data, BadRequest("Invalid parameters"))
      _ <- productService.updateProduct(productId, form).leftMap(err => BadRequest(ApiError(err.code, err.errorMessage).toJson))
    } yield Ok(ApiResponse.empty.toJson)

    program
      .valueOr(err => err)
      .unsafeToFuture
  }

  def bulkEnable = authAction(parse.json).async { req =>
    val ids: Option[List[Long]] = (req.body \ "productIds").asOpt[List[Long]]
    val program: OptionT[IO, Result] = for {
      productIds <- OptionT.fromOption[IO](ids)
      affected <- OptionT.liftF(productWriteRepo.bulkUpdateProduct(productIds, Map("status" -> "1")))
    } yield Ok(s"$affected")

    program
      .getOrElse(BadRequest("Invalid `productIds` parameter"))
      .unsafeToFuture
  }

  def bulkDisable() = authAction(parse.json).async { req =>
    val ids: Option[List[Long]] = (req.body \ "productIds").asOpt[List[Long]]
    val program: OptionT[IO, Result] = for {
      productIds <- OptionT.fromOption[IO](ids)
      affected <- OptionT.liftF(productWriteRepo.bulkUpdateProduct(productIds, Map("status" -> "0")))
    } yield Ok(s"$affected")

    program
      .getOrElse(BadRequest("Invalid `productIds` parameter"))
      .unsafeToFuture
  }

  def bulkAddAttributes = authAction(parse.json).async { req =>
    val json = req.body
    val productIds = (json \ "productIds").asOpt[List[Int]]
    val attributes = (json \ "attributes").asOpt[List[AttributeIdValuePair]]
    val error: DomainError = GenericError

    val program: EitherT[IO, DomainError, Unit] = for {
      ids <- EitherT.fromOption[IO](productIds, error)
      pairs <- EitherT.fromOption[IO](attributes, error)
      _ <- ids.traverse(
        id => productService.addProductAttributes(id, pairs)
      )
    } yield ()

    program
      .map(_ => Ok)
      .valueOr(err => BadRequest(ApiError.fromDomainError(err).toJson))
      .unsafeToFuture
  }
}
