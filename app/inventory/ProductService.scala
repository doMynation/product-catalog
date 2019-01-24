package inventory

import java.sql.Connection
import javax.inject.Inject
import inventory.dtos.{AttributeIdValuePair, ProductAttributeDTO}
import inventory.entities.{Attribute, Product, ProductAttribute}
import inventory.forms.EditProductForm
import inventory.repositories.{MiscRepository, ProductInclusions, ProductRepository, ProductWriteRepository}
import inventory.validators.{DomainError, InvalidHash}
import play.api.db.Database
import scala.util.{Failure, Random, Try}

final class ProductService @Inject()(readRepository: ProductRepository, writeRepository: ProductWriteRepository, miscRepository: MiscRepository, db: Database) {

  def updateProduct(productId: Long, form: EditProductForm): Either[DomainError, Unit] =
    readRepository
      .get(productId, "en")
      .filter(_.hash == form.hash)
      .map(product => form
        .validate(readRepository, miscRepository)
        .map(dto => writeRepository.updateProduct(product, dto))
      ).getOrElse(Left(InvalidHash))

  def cloneProduct(productId: Long): Try[Product] = {
    // Fetch the product's info
    val includes = List(ProductInclusions.ATTRIBUTES, ProductInclusions.CHILDREN, ProductInclusions.RULES, ProductInclusions.ASSEMBLY_PARTS)
    val productOpt = readRepository.get(productId, "en", includes)

    if (productOpt.isEmpty)
      return Failure(new RuntimeException("Product not found"))

    val product = productOpt.get
    val newSku = s"${product.sku}_COPIE_${Random.alphanumeric.take(5).mkString("")}"
    val dto = product.toDto.copy(sku = newSku)
    val translationDtos = readRepository.getTranslations(product.descriptionId).map(_.toDto)
    val storePricesDtos = readRepository.getProductStorePrices(productId).map(_.toDto)

    val dtoAugmented = dto.copy(
      translations = translationDtos,
      storePrices = storePricesDtos
    )

    writeRepository.createProduct(dtoAugmented).map { newProductId =>
      readRepository.get(newProductId, "en").get
    }
  }

  def addProductAttributes(productId: Long, attributeIdValuePairs: Seq[AttributeIdValuePair]): Boolean = db.withTransaction { implicit conn =>
    attributeIdValuePairs.foreach { pair =>
      // Get the attribute
      val ao: Option[Attribute] = readRepository.getAttribute(pair.id, "en")

      ao map { attribute =>
        val dto = ProductAttributeDTO(
          attribute.id,
          pair.value,
          None,
          pair.isEditable,
          attribute.inputType == "select")

        insertOrReplaceProductAttribute(productId, dto)
      } getOrElse {
        throw new RuntimeException(s"Attribute ${pair.id} does not exist")
      }
    }

    true
  }

  private def insertOrReplaceProductAttribute(productId: Long, dto: ProductAttributeDTO)(implicit conn: Connection): Unit = {
    // Check if the product already has that attribute
    val pao: Option[ProductAttribute] = readRepository.getProductAttribute(productId, dto.attributeId, "en")

    pao.map { pa =>
      writeRepository.updateProductAttribute(pa.id, dto).toString
    } getOrElse {
      // Add a new attribute
      writeRepository.createProductAttribute(productId, dto)
    }
  }
}
