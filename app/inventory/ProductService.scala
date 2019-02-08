package inventory

import javax.inject.Inject
import inventory.dtos.{AttributeIdValuePair, ProductAttributeDTO, ProductDTO}
import inventory.entities.{Attribute, Product, ProductAttribute}
import inventory.forms.EditProductForm
import inventory.repositories._
import inventory.validators._
import play.api.db.Database
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import cats.data.{EitherT, OptionT}
import cats.implicits._
import shared.Types.ServiceResponse

final class ProductService @Inject()(readRepo: ProductRepository, writeRepository: ProductWriteRepository, miscRepository: MiscRepository, db: Database)(implicit ec: ExecutionContext) {

  def updateProduct(productId: Long, form: EditProductForm): ServiceResponse[Unit] = {
    (for {
      product <- EitherT.fromOptionF(readRepo.getById(productId, "en"), ProductNotFound(productId)) // Fetch product
      _ <- EitherT.cond[Future](product.hash == form.hash, product.hash, InvalidHash) // Check hash
      dto <- EitherT(form.validate(readRepo, miscRepository)) // Validate form
      _ <- EitherT.rightT[Future, DomainError](writeRepository.updateProduct(product, dto)) // Update product
    } yield ()).value
  }

  def cloneProduct(productId: Long): ServiceResponse[Product] = {
    val includes = List(ProductInclusions.ATTRIBUTES, ProductInclusions.CHILDREN, ProductInclusions.RULES, ProductInclusions.ASSEMBLY_PARTS)
    val error: DomainError = GenericError

    val program = for {
      product <- EitherT.fromOptionF(readRepo.getById(productId, "en", includes), error) // Fetch the product
      dto <- EitherT.right[DomainError](prepDto(product)) // Convert to DTO
      newProductId <- EitherT.fromOption[Future](writeRepository.createProduct(dto).toOption, error) // Create product
      newProduct <- EitherT.fromOptionF(readRepo.getById(newProductId, "en", includes), error) // Fetch the new product
    } yield newProduct

    program.value
  }

  def addProductAttributes(productId: Long, attributeIdValuePairs: List[AttributeIdValuePair]): ServiceResponse[Unit] = {
    val error: DomainError = InvalidAttributes
    val pairsWithAttribute: EitherT[Future, DomainError, List[(AttributeIdValuePair, Attribute)]] = attributeIdValuePairs.traverse { pair =>
      for {
        attr <- EitherT.fromOptionF(readRepo.getAttribute(pair.id, "en"), error)
      } yield (pair, attr)
    }

    val program: EitherT[Future, DomainError, Unit] = for {
      tuples <- pairsWithAttribute
      _ <- EitherT.right[DomainError](tuples.traverse { tuple =>
        val (pair, attr) = tuple
        val dto = ProductAttributeDTO(
          attr.id,
          pair.value,
          None,
          pair.isEditable,
          attr.inputType == "select"
        )

        insertOrReplaceProductAttribute(productId, dto)
      })
    } yield ()

    program.value
  }

  private def insertOrReplaceProductAttribute(productId: Long, dto: ProductAttributeDTO): Future[Unit] = {
    // Check if the product already has that attribute
    val pao: Future[Option[ProductAttribute]] = readRepo.getProductAttribute(productId, dto.attributeId, "en")

    OptionT(pao).map { pa =>
      db.withConnection(implicit conn => writeRepository.updateProductAttribute(pa.id, dto))
      () // @todo: Fix this
    } getOrElse {
      // Add a new attribute
      db.withConnection(implicit conn => writeRepository.createProductAttribute(productId, dto))
      () // @todo: Fix this
    }
  }

  private def prepDto(product: Product): Future[ProductDTO] = {
    val newSku = s"${product.sku}_COPIE_${Random.alphanumeric.take(5).mkString("")}"
    val dto = product.toDto.copy(sku = newSku)
    val translationsF = readRepo.getTranslations(product.descriptionId)
    val storePricesF = readRepo.getProductStorePrices(product.id)

    for {
      translations <- translationsF
      storePrices <- storePricesF
    } yield dto.copy(
      translations = translations.map(_.toDto),
      storePrices = storePrices.map(_.toDto)
    )
  }
}
