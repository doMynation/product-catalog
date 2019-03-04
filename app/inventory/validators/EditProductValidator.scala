package inventory.validators

import inventory.dtos._
import inventory.forms.EditProductForm
import inventory.repositories.{MiscRepository, ProductReadRepository}
import shared.dtos.TranslationDTO
import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._
import scala.concurrent.{ExecutionContext}
import scala.util.Try

final class EditProductValidator {
  //  storePrices: Seq[ProductStorePriceDTO] = List(),
  //  assemblyParts: Seq[ProductAssemblyPartDTO] = List(),

  def validateHash(value: String): Either[DomainError, String] =
    Either.cond(value == "PRISTINE" || value.length > 10, value, InvalidHash)

  def validateSku(value: String): Either[DomainError, String] =
    Either.cond(value.matches("^[\\w-]+$"), value, InvalidSku)

  def validateCategoryId(value: Long, pr: ProductReadRepository): EitherT[IO, DomainError, Long] = {
    val error: DomainError = InvalidCategoryId

    for {
      _ <- EitherT.cond[IO](value > 0, value, error)
      _ <- EitherT.fromOptionF(pr.getProductCategory(value, "en"), error)
    } yield value
  }

  def validateDepartmentId(value: Option[Long], pr: ProductReadRepository): EitherT[IO, DomainError, Option[Long]] = {
    val error: DomainError = InvalidDepartmentId

    for {
      _ <- EitherT.cond[IO](value.isEmpty || value.get > 0, value, error)
      _ <- EitherT.fromOptionF(pr.getProductDepartment(value.get, "en"), error)
    } yield value
  }

  def validatePrice(value: Double): Either[DomainError, Double] =
    Either.cond(
      value >= 0,
      value,
      InvalidPrice
    )

  def validateTranslations(value: Seq[TranslationDTO]): Either[DomainError, Seq[TranslationDTO]] =
    Either.cond(
      value.forall(_.name.nonEmpty) && // all names are non-empty
        value.filter(_.isDefault).length == 1, // there is exactly one default translation
      value,
      InvalidTranslations
    )

  def validateTags(value: Seq[String]): Either[DomainError, Seq[String]] =
    Either.cond(
      value.distinct.size == value.length,
      value,
      InvalidTags
    )

  def validateAttributes(value: List[AttributeIdValuePair], pr: ProductReadRepository): EitherT[IO, DomainError, List[ProductAttributeDTO]] = {
    // @todo: Validate value based on attribute's data and input type
    // @todo: Validate existence of value (if reference)

    val error: DomainError = InvalidAttributes
    value.traverse { pair =>
      for {
        attr <- EitherT.fromOptionF(pr.getAttribute(pair.id, "en"), error)
      } yield ProductAttributeDTO(
        attr.id,
        pair.value,
        None,
        pair.isEditable,
        attr.inputType == "select"
      )
    }
  }

  def validateChildren(value: List[ProductChildDTO], pr: ProductReadRepository): EitherT[IO, DomainError, List[ProductChildDTO]] = {
    val error: Long => DomainError = ProductNotFound(_)

    value.traverse(child =>
      for {
        _ <- EitherT.fromEither[IO](child.validate)
        _ <- EitherT.fromOptionF(pr.getById(child.productId, "en"), error(child.productId))
      } yield child
    )
  }

  def validateSalesRules(value: List[ProductRuleDTO], pr: ProductReadRepository): EitherT[IO, DomainError, List[ProductRuleDTO]] = {
    val error: Long => DomainError = ProductNotFound(_)

    value.traverse { rule =>
      for {
        _ <- EitherT.fromEither[IO](rule.validate)
        _ <- EitherT.fromOptionF(pr.getById(rule.productId, "en"), error(rule.productId))
      } yield rule
    }
  }

  def validateMetadata(value: Map[String, String], mr: MiscRepository): EitherT[IO, DomainError, Map[String, String]] = {
    val isKit: Either[DomainError, String] = value.get("isKit").filter(v => v == "1" || v == "0").toRight(InvalidMetadata("isKit"))
    val extrusionId = value.get("extrusionId")
      .map(id => validateExtrusion(id, mr))
      .getOrElse(EitherT.rightT[IO, DomainError](""))
    val stickerId: Either[DomainError, String] = value.get("stickerId") match {
      case Some("") => Right("")
      case Some(id) if !Set("1", "2").contains(id) => Left(InvalidMetadata("stickerId"))
      case _ => Right("")
    }

    for {
      _ <- extrusionId
      _ <- EitherT.fromEither[IO](isKit)
      _ <- EitherT.fromEither[IO](stickerId)
    } yield value
  }

  def validateExtrusion(id: String, mr: MiscRepository): EitherT[IO, DomainError, String] = {
    if (id.trim.isEmpty) return EitherT.rightT[IO, DomainError](id)

    val error: DomainError = InvalidMetadata("extrusionId")

    for {
      extId <- EitherT.fromEither[IO](Try(id.toLong).map(Right(_)).getOrElse(Left(error)))
      _ <- EitherT.fromOptionF(mr.getExtrusion(extId), error)
    } yield id
  }

  def validate(form: EditProductForm, pr: ProductReadRepository, mr: MiscRepository): EitherT[IO, DomainError, ProductDTO] =
    for {
      _ <- EitherT.fromEither[IO](validateHash(form.hash))
      _ <- EitherT.fromEither[IO](validateSku(form.sku))
      _ <- validateCategoryId(form.categoryId, pr)
      _ <- validateDepartmentId(form.departmentId, pr)
      _ <- EitherT.fromEither[IO](validateTranslations(form.translations))
      _ <- EitherT.fromEither[IO](validatePrice(form.price))
      _ <- EitherT.fromEither[IO](validatePrice(form.costPrice))
      _ <- EitherT.fromEither[IO](validateTags(form.tags))
      attributeDtos <- validateAttributes(form.attributes.toList, pr)
      _ <- validateChildren(form.children.toList, pr)
      _ <- validateMetadata(form.metadata, mr)
      _ <- validateSalesRules(form.rules.toList, pr)
    } yield form.toDto.copy(attributes = attributeDtos)
}

