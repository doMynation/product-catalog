package sales

import javax.inject.Inject
import cats.data.OptionT
import cats.effect.IO
import cats.implicits._
import inventory.repositories.{ProductReadRepository}
import shared.entities.LineItem

final class SalesService @Inject()(productRepo: ProductReadRepository) {

  def populateLineItem(item: LineItem, lang: String, includes: Seq[String] = List()): IO[LineItem] = {
    val overridenLineItem: OptionT[IO, LineItem] = for {
      productId <- OptionT.fromOption[IO](item.productId)
      product <- OptionT(productRepo.getById(productId, lang, includes))
      productWithOverrides <- OptionT.liftF(productRepo.applyProductAttributeOverrides(product, item.attributeOverrides, lang))
    } yield item.copy(product = Some(productWithOverrides))

    overridenLineItem.getOrElse(item)
  }

}
