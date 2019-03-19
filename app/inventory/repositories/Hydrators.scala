package inventory.repositories

import java.sql.ResultSet
import java.time.LocalDateTime
import inventory.entities._
import inventory.util.DB
import shared.entities.Lang

object Hydrators {
  def hydrateAttributeValue(rs: ResultSet): AttributeValue = {
    val ts = rs.getTimestamp("modification_date")
    val updatedAt = if (rs.wasNull()) None else Option(ts.toLocalDateTime)

    AttributeValue(
      rs.getLong("id"),
      rs.getString("sku"),
      Description(
        rs.getString("name"),
        rs.getString("short_description"),
        rs.getString("long_description")
      ),
      rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt
    )
  }

  def hydrateProduct(rs: ResultSet): Product = {
    val metadata = Map(
      "mpn" -> DB.getNullable[String]("mpn", rs).getOrElse(""),
      "imageUrl" -> DB.getNullable[String]("image_url", rs).getOrElse(""),
      "isKit" -> rs.getInt("is_kit").toString,
      "stickerId" -> DB.getNullable[String]("sticker_template_id", rs).getOrElse(""),
      "extrusionId" -> DB.getNullable[String]("extrusion_template_id", rs).getOrElse(""),
    )

    val tags = DB.getNullable[String]("tags", rs) match {
      case Some("") => List[String]()
      case Some(s) => s.split(",").toList
      case _ => List[String]()
    }

    val department = DB.getNullable[String]("d.code", rs).map { _ =>
      hydrateProductDepartment(rs)
    }

    Product(
      rs.getLong("id"),
      rs.getString("hash"),
      rs.getLong("category_id"),
      rs.getString("sku"),
      rs.getLong("p.description_id"),
      Description(rs.getString("p.name"), rs.getString("p.short_description"), rs.getString("p.long_description")),
      rs.getDouble("price"),
      rs.getDouble("cost_price"),
      tags = tags,
      category = Some(hydrateProductCategory(rs)),
      department = department,
      createdAt = rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt = DB.getNullable[LocalDateTime]("modification_date", rs),
      metadata = metadata,
      isCustom = rs.getBoolean("is_custom"),
      isEnabled = rs.getBoolean("status"),
    )
  }

  def hydrateProductAttribute(rs: ResultSet): ProductAttribute = {
    ProductAttribute(
      id = rs.getLong("record_id"),
      attribute = Attribute(
        rs.getLong("attribute_id"),
        rs.getString("attribute_code"),
        rs.getString("attribute_data_type"),
        rs.getString("attribute_input_type"),
        Description(
          rs.getString("attribute_name"),
          rs.getString("attribute_short_description"),
          rs.getString("attribute_long_description")
        ),
        rs.getTimestamp("attribute_creation_date").toLocalDateTime,
        DB.getNullable[LocalDateTime]("attribute_modification_date", rs)
      ),
      value = rs.getString("value"),
      valueId = DB.getNullable[Long]("value_id", rs),
      valueSku = DB.getNullable[String]("value_sku", rs),
      isEditable = rs.getBoolean("is_editable"),
      isReference = rs.getBoolean("is_reference")
    )
  }

  def hydrateProductCategory(rs: ResultSet): ProductCategory = {
    ProductCategory(
      rs.getLong("c.id"),
      rs.getString("c.code"),
      Description(
        rs.getString("c.name"),
        rs.getString("c.short_description"),
        rs.getString("c.long_description")
      ),
      createdAt = rs.getTimestamp("c.creation_date").toLocalDateTime,
      updatedAt = DB.getNullable[LocalDateTime]("c.modification_date", rs)
    )
  }

  def hydrateProductDepartment(rs: ResultSet): ProductDepartment = {
    ProductDepartment(
      rs.getLong("d.id"),
      rs.getString("d.code"),
      Description(
        rs.getString("d.name"),
        rs.getString("d.short_description"),
        rs.getString("d.long_description")
      ),
      createdAt = rs.getTimestamp("d.creation_date").toLocalDateTime,
      updatedAt = DB.getNullable[LocalDateTime]("d.modification_date", rs)
    )
  }

  def productRuleExtractor(rs: ResultSet) = (
    rs.getLong("related_product_id"),
    rs.getLong("id"),
    rs.getDouble("price"),
    rs.getString("type"),
    rs.getInt("quantity"),
    rs.getInt("max_quantity")
  )

  def hydrateTranslation(rs: ResultSet): Translation =
    Translation(
      id = rs.getLong("id"),
      lang = Lang.fromId(rs.getInt("lang_id"), "fr"),
      description = hydrateDescription(rs),
      isDefault = rs.getBoolean("is_default")
    )

  def hydrateDescription(rs: ResultSet): Description =
    Description(
      rs.getString("label"),
      rs.getString("short_description"),
      rs.getString("long_description")
    )

  def hydrateAttribute(rs: ResultSet): Attribute =
    Attribute(
      rs.getLong("id"),
      rs.getString("code"),
      rs.getString("data_type"),
      rs.getString("input_type"),
      hydrateDescription(rs),
      rs.getTimestamp("creation_date").toLocalDateTime,
      DB.getNullable[LocalDateTime]("modification_date", rs)
    )
}
