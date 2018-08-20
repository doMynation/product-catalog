package inventory

import javax.inject.Inject

import inventory.entities.Product
import inventory.repositories.{ProductInclusions, ProductRepository, ProductWriteRepository}

import scala.util.{Failure, Random, Try}

final class ProductService @Inject()(readRepository: ProductRepository, writeRepository: ProductWriteRepository) {

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
    // @todo: Attributes, children, rules

    val dtoAugmented = dto.copy(
      translations = translationDtos,
      storePrices = storePricesDtos
    )

    //      for {
    //        productId <- writeRepository.createProduct(dtoWithTranslations)
    //        otherProductId <- writeRepository.createAttributes(productId)
    //        //        lastProduct <- writeRepository.createRules(productId)
    //      } yield lastProduct

    writeRepository.createProduct(dtoAugmented).map { newProductId =>
      readRepository.get(newProductId, "en").get
    }
  }
}
