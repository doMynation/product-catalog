package inventory.validators

sealed trait DomainError {
  val code: String
  val errorMessage: String
}

case object InvalidHash extends DomainError {
  override val errorMessage = "The hash does not match the current version."
  override val code = "INVALID_HASH"
}

case object InvalidSku extends DomainError {
  override val errorMessage = "SKU cannot be empty, and must consist of alphanumeric, hyphens and underscores only."
  override val code = "INVALID_SKU"
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
