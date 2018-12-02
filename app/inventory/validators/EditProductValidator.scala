package inventory.validators

import inventory.dtos.{AttributeIdValuePair, ProductAttributeDTO, ProductChildDTO, ProductDTO}
import inventory.forms.EditProductForm
import inventory.repositories.{MiscRepository, ProductRepository}
import shared.dtos.TranslationDTO
import cats.data.OptionT
import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.option._
import cats.instances.list._
import cats.instances._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.{Success, Try}

object EditProductValidator {
  //  storePrices: Seq[ProductStorePriceDTO] = List(),
  //  rules: Seq[ProductRuleDTO] = List(),
  //  assemblyParts: Seq[ProductAssemblyPartDTO] = List(),

  def validateHash(value: String): Either[DomainError, String] =
    Either.cond(value == "PRISTINE" || value.length > 10, value, InvalidHash)

  def validateSku(value: String): Either[DomainError, String] =
    Either.cond(value.matches("^[\\w-]+$"), value, InvalidSku)

  def validateCategoryId(value: Long, pr: ProductRepository): Either[DomainError, Long] =
    Either.cond(
      value > 0 && pr.getProductCategory(value, "en").isDefined,
      value,
      InvalidCategoryId
    )

  def validateDepartmentId(value: Option[Long], pr: ProductRepository): Either[DomainError, Option[Long]] =
    Either.cond(
      value.isEmpty || value.filter(_ > 0).flatMap(pr.getProductDepartment(_, "en")).isDefined,
      value,
      InvalidDepartmentId
    )

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

  def validateAttributes(value: List[AttributeIdValuePair], productRepository: ProductRepository): Either[DomainError, List[ProductAttributeDTO]] = {
    // @todo: Validate value based on attribute's data and input type
    // @todo: Validate existence of value (if reference)
    value.traverse(pair =>
      productRepository
        .getAttribute(pair.id, "en")
        .map(attr => ProductAttributeDTO(
          attr.id,
          pair.value,
          None,
          pair.isEditable,
          attr.inputType == "select"
        ))
        .toRight(InvalidAttributes)
    )
  }

  def validateChildren(value: List[ProductChildDTO], productRepository: ProductRepository): Either[DomainError, List[ProductChildDTO]] = {
    value.traverse(child =>
      // @todo: Validate childType
      if (child.quantity < 1) Left(InvalidChildren)
      else productRepository
        .get(child.productId, "en")
        .map(s => child)
        .toRight(InvalidChildren)
    )
  }

  def validateMetadata(value: Map[String, String], mr: MiscRepository): Either[DomainError, Map[String, String]] = {
    val isKit = value.get("isKit").filter(v => v == "1" || v == "0").toRight(InvalidMetadata("isKit"))
    val extrusionId = value.get("extrusionId").map(id => validateExtrusion(id, mr)).getOrElse(Right(value))
    val stickerId = value.get("stickerId") match {
      case Some("") => Right(value)
      case Some(id) if !Set("1", "2").contains(id) => Left(InvalidMetadata("stickerId"))
      case _ => Right(value)
    }

    for {
      _ <- isKit
      _ <- extrusionId
      _ <- stickerId
    } yield value
  }

  def validateExtrusion(id: String, mr: MiscRepository): Either[DomainError, String] = {
    if (id.trim.isEmpty) return Right(id)

    val long = Try(id.toLong)

    long match {
      case Success(extrusionId) =>
        val f = mr.getExtrusion(extrusionId)
        val e = Await.result(f, 3 second)
        e.map(_ => id).toRight(InvalidMetadata("extrusionId"))
      case _ => Left(InvalidMetadata("extrusionId"))
    }
  }

  def validate(form: EditProductForm, pr: ProductRepository, mr: MiscRepository): Either[DomainError, ProductDTO] = {
    for {
      _ <- validateHash(form.hash)
      _ <- validateSku(form.sku)
      _ <- validateCategoryId(form.categoryId, pr)
      _ <- validateDepartmentId(form.departmentId, pr)
      _ <- validateTranslations(form.translations)
      _ <- validatePrice(form.price)
      _ <- validatePrice(form.costPrice)
      _ <- validateTags(form.tags)
      attributeDtos <- validateAttributes(form.attributes.toList, pr)
      _ <- validateChildren(form.children.toList, pr)
      _ <- validateMetadata(form.metadata, mr)
    } yield form.toDto.copy(attributes = attributeDtos)
  }
}
