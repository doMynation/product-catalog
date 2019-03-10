package inventory

import javax.inject.Inject
import inventory.dtos.{AttributeIdValuePair, ProductAttributeDTO, ProductDTO}
import inventory.entities.{Attribute, Product, ProductAttribute}
import inventory.forms.EditProductForm
import inventory.repositories._
import inventory.validators._
import play.api.db.Database
import scala.util.Random
import cats.data.{EitherT}
import cats.effect.{ContextShift, IO}
import cats.effect.implicits._
import cats.implicits._
import shared.Types.{ServiceResponse}

final class ProductService @Inject()(
                                      doobieRepo: ProductReadRepository,
                                      writeRepository: ProductWriteRepository,
                                      miscRepository: MiscRepository,
                                      db: Database
                                    )(implicit cs: ContextShift[IO]) {

  def updateProduct(productId: Long, form: EditProductForm): EitherT[IO, DomainError, Unit] = {
    for {
      product <- EitherT.fromOptionF(doobieRepo.getById(productId, "en"), ProductNotFound(productId)) // Fetch product
      _ <- EitherT.cond[IO](product.hash == form.hash, product.hash, InvalidHash) // Check hash
      dto <- form.validate(doobieRepo, miscRepository) // Validate form
      _ <- EitherT.right[DomainError](writeRepository.updateProduct(product, dto)) // Update product
    } yield ()
  }

  def cloneProduct(productId: Long): EitherT[IO, DomainError, Product] = {
    val includes = List(ProductInclusions.ATTRIBUTES, ProductInclusions.CHILDREN, ProductInclusions.RULES, ProductInclusions.ASSEMBLY_PARTS)
    val error: DomainError = GenericError

    for {
      product <- EitherT.fromOptionF(doobieRepo.getById(productId, "en", includes), error) // Fetch the product
      dto <- EitherT.right[DomainError](prepCloneDto(product)) // Convert to DTO
      newProductId <- EitherT.right[DomainError](writeRepository.createProduct(dto)) // Create product
      newProduct <- EitherT.fromOptionF(doobieRepo.getById(newProductId, "en", includes), error) // Fetch the new product
    } yield newProduct
  }

  def addProductAttributes(productId: Long, attributeIdValuePairs: List[AttributeIdValuePair]): ServiceResponse[Unit] = {
    val error: DomainError = InvalidAttributes
    val pairsWithAttribute: EitherT[IO, DomainError, List[(AttributeIdValuePair, Attribute)]] =
      attributeIdValuePairs.traverse { pair =>
        for {
          attr <- EitherT.fromOptionF(doobieRepo.getAttribute(pair.id, "en"), error)
        } yield (pair, attr)
      }

    for {
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
  }

  private def insertOrReplaceProductAttribute(productId: Long, dto: ProductAttributeDTO): IO[Unit] = {
    // Check if the product already has that attribute
    val pao: IO[Option[ProductAttribute]] = doobieRepo.getProductAttribute(productId, dto.attributeId, "en")

    pao.flatMap {
      case Some(pa) => writeRepository.updateProductAttribute(pa.id, dto).map(_ => ())
      case _ => writeRepository.createProductAttribute(productId, dto).map(_ => ())
    }
  }

  private def prepCloneDto(product: Product): IO[ProductDTO] = {
    val newSku = s"${product.sku}_COPIE_${Random.alphanumeric.take(5).mkString("")}"
    val dto = product.toDto.copy(sku = newSku)
    val tIO = doobieRepo.getTranslations(product.descriptionId)
    val spIO = doobieRepo.getProductStorePrices(product.id)

    // Fetch translations and store prices in parallel
    val dtoIO: IO[ProductDTO] = (tIO, spIO).parMapN { (translations, storePrices) =>
      dto.copy(
        translations = translations.map(_.toDto),
        storePrices = storePrices.map(_.toDto)
      )
    }

    dtoIO
  }
}
