package inventory.validators

sealed trait DomainError {
  val code: String
  val errorMessage: String
}

case object GenericError extends DomainError {
  override val errorMessage = ""
  override val code = "ERROR"
}

case object InvalidPasswordResetToken extends DomainError {
  override val errorMessage = "Invalid password reset token."
  override val code = "INVALID_PASSWORD_RESET_TOKEN"
}

case object InvalidPassword extends DomainError {
  override val errorMessage = "Invalid password."
  override val code = "INVALID_PASSWORD"
}

case object InvalidPayload extends DomainError {
  override val errorMessage = "The payload is malformed."
  override val code = "INVALID_PAYLOAD"
}

case object InvalidHash extends DomainError {
  override val errorMessage = "The hash does not match the current version."
  override val code = "INVALID_HASH"
}

case object InvalidName extends DomainError {
  override val errorMessage = s"The name cannot be empty.";
  override val code = "INVALID_NAME"
}

case class InvalidLanguage(lang: String) extends DomainError {
  override val errorMessage = s"The language $lang is invalid.";
  override val code = "INVALID_LANG"
}

case object InvalidCode extends DomainError {
  override val errorMessage = s"The code cannot be empty.";
  override val code = "INVALID_CODE"
}

case object InvalidSku extends DomainError {
  override val errorMessage = "The SKU cannot be empty, and must consist of alphanumeric, hyphens and underscores only."
  override val code = "INVALID_SKU"
}

case object InvalidProductId extends DomainError {
  override val errorMessage = "The product ID is invalid."
  override val code = "INVALID_PRODUCT_ID"
}

case object InvalidCategoryId extends DomainError {
  override val errorMessage = "The category doesn't exist."
  override val code = "INVALID_CATEGORY"
}

case object InvalidDepartmentId extends DomainError {
  override val errorMessage = "The department doesn't exist."
  override val code = "INVALID_DEPARTMENT"
}

case object InvalidPrice extends DomainError {
  override val errorMessage = "The price must be equal or greater than 0.";
  override val code = "INVALID_PRICE"
}

case object InvalidTranslations extends DomainError {
  override val errorMessage = "All translations must have a name that isn't empty. In addition, there can only be one default translation.";
  override val code = "INVALID_TRANSLATIONS"
}

case object InvalidTags extends DomainError {
  override val errorMessage = "All tags must be unique. Duplicates are not allowed.";
  override val code = "INVALID_TAGS"
}

case object InvalidAttributes extends DomainError {
  override val errorMessage = "All attributes must exist and have valid values.";
  override val code = "INVALID_ATTRIBUTES"
}

case object InvalidChildren extends DomainError {
  override val errorMessage = "All children must exist and have valid values.";
  override val code = "INVALID_CHILDREN"
}

case object InvalidChildType extends DomainError {
  override val errorMessage = "The child type is invalid";
  override val code = "INVALID_CHILD_TYPE"
}

case class InvalidMetadata(code: String) extends DomainError {
  override val errorMessage = s"Metadata ${code} is not valid.";
}

case object InvalidSalesRuleType extends DomainError {
  override val errorMessage = s"Invalid sales rule type";
  override val code = "INVALID_SALES_RULE_TYPE"
}

case object InvalidQuantity extends DomainError {
  override val errorMessage = s"The quantity must be greater than 0.";
  override val code = "INVALID_QUANTITY"
}

case class ProductNotFound(productId: Long) extends DomainError {
  override val errorMessage = s"Product ID $productId not found";
  override val code = "PRODUCT_NOT_FOUND"
}

case class UserNotFound(email: String) extends DomainError {
  override val errorMessage = s"User not found with address ${email}";
  override val code = "USER_NOT_FOUND"
}

case class DepartmentNotFound(departmentId: Long) extends DomainError {
  override val errorMessage = s"Department ID $departmentId not found";
  override val code = "DEPARTMENT_NOT_FOUND"
}

