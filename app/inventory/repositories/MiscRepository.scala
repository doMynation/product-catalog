package inventory.repositories

import cats.effect.{ContextShift, IO}
import javax.inject.Inject
import inventory.entities.Extrusion
import shared.Types.Tx
import doobie._
import doobie.implicits._

final class MiscRepository @Inject()(db: Tx)(implicit cs: ContextShift[IO]) {

  def getExtrusion(id: Long): IO[Option[Extrusion]] = {
    val sql =
      sql"""
         SELECT
         et.id,
         e.supplier_code AS mpn,
         e.name,
         et.name AS templateName,
         et.image_url AS imageUrl,
         e.profile_image_url AS profileImageUrl
         FROM extrusion_templates et JOIN extrusions e ON e.id = et.extrusion_id
         WHERE et.id = $id
      """
    val query = sql.query[Extrusion].option

    db.use(query.transact(_))
  }

  def getExtrusions: IO[List[Extrusion]] = {
    val sql =
      sql"""
         SELECT
         et.id,
         e.supplier_code AS mpn,
         e.name AS name,
         et.name AS templateName,
         et.image_url AS imageUrl,
         e.profile_image_url AS profileImageUrl
         FROM extrusion_templates et JOIN extrusions e ON e.id = et.extrusion_id
      """
    val query = sql.query[Extrusion].to[List]

    db.use(query.transact(_))
  }
}
