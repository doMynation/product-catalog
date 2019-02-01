package sales

import javax.inject.Inject
import cats.data.OptionT
import cats.implicits._
import inventory.repositories.ProductRepository2
import shared.LineItem
import scala.concurrent.{ExecutionContext, Future}

final class SalesService @Inject()(productRepo: ProductRepository2)(implicit ec: ExecutionContext) {

  def populateLineItem(item: LineItem, lang: String, includes: Seq[String] = Seq()): Future[LineItem] = {
    val overridenLineItem: OptionT[Future, LineItem] = for {
      productId <- OptionT.fromOption[Future](item.productId)
      product <- OptionT(productRepo.getById(productId, lang, includes))
      productWithOverrides <- OptionT.liftF(productRepo.applyProductAttributeOverrides(product, item.attributeOverrides, lang))
    } yield item.copy(product = Some(productWithOverrides))

    overridenLineItem.getOrElse(item)
  }

}
