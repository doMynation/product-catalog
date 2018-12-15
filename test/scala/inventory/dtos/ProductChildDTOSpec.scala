package scala.inventory.dtos

import inventory.dtos.ProductChildDTO
import inventory.entities.ProductChild
import inventory.validators.{InvalidChildType, InvalidProductId, InvalidQuantity}
import org.scalatestplus.play.PlaySpec

class ProductChildDTOSpec extends PlaySpec {
  "A valid product child" must {
    "have a natural number as ID" in {
      ProductChildDTO(-1, ProductChild.TYPE_USED, 1, true, true).validate mustBe Left(InvalidProductId)
      ProductChildDTO(0, ProductChild.TYPE_USED, 1, true, true).validate mustBe Left(InvalidProductId)
    }

    "have a valid type" in {
      ProductChildDTO(1, "BAD_TYPE", 1, true, true).validate mustBe Left(InvalidChildType)
      ProductChildDTO(1, "", 1, true, true).validate mustBe Left(InvalidChildType)
    }

    "have a natural number as the quantity" in {
      ProductChildDTO(1, ProductChild.TYPE_USED, 0, true, true).validate mustBe Left(InvalidQuantity)
      ProductChildDTO(1, ProductChild.TYPE_USED, -1, true, true).validate mustBe Left(InvalidQuantity)
    }
  }
}
