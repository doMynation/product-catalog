package shared.dtos

case class TranslationDTO(
                           lang: String,
                           name: String,
                           shortDescription: String = "",
                           longDescription: String = "",
                           isDefault: Boolean = false)
