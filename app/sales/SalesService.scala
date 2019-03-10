package sales

import javax.inject.Inject
import cats.data.OptionT
import cats.effect.IO
import cats.implicits._
import inventory.repositories.{ProductReadRepository, ProductRepository}
import shared.entities.LineItem

import scala.concurrent.{ExecutionContext, Future}

final class SalesService @Inject()(doobieRepo: ProductReadRepository, productRepo: ProductRepository)(implicit ec: ExecutionContext) {

  def populateLineItem(item: LineItem, lang: String, includes: Seq[String] = Seq()): Future[LineItem] = {
    val overridenLineItem: OptionT[Future, LineItem] = for {
      productId <- OptionT.fromOption[Future](item.productId)
      product <- OptionT(productRepo.getById(productId, lang, includes))
      productWithOverrides <- OptionT.liftF(productRepo.applyProductAttributeOverrides(product, item.attributeOverrides, lang))
    } yield item.copy(product = Some(productWithOverrides))

    overridenLineItem.getOrElse(item)
  }

  def populateLineItemIO(item: LineItem, lang: String, includes: Seq[String] = List()): IO[LineItem] = {
    val overridenLineItem: OptionT[IO, LineItem] = for {
      productId <- OptionT.fromOption[IO](item.productId)
      product <- OptionT(doobieRepo.getById(productId, lang, includes))
      productWithOverrides <- OptionT.liftF(doobieRepo.applyProductAttributeOverrides(product, item.attributeOverrides, lang))
    } yield item.copy(product = Some(productWithOverrides))

    overridenLineItem.getOrElse(item)
  }

}
