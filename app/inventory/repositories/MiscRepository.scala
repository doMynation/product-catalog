package inventory.repositories

import java.sql.ResultSet
import javax.inject.Inject
import infra.DatabaseExecutionContext
import inventory.entities.Extrusion
import inventory.util.DB
import play.api.db.Database
import scala.concurrent.Future

final class MiscRepository @Inject()(db: Database)(implicit ec: DatabaseExecutionContext) {

  def getExtrusion(id: Long): Future[Option[Extrusion]] = Future {
    val sql =
      """
         SELECT
         et.id,
         e.supplier_code AS mpn,
         et.name AS templateName,
         e.name AS name,
         et.image_url AS imageUrl,
         e.profile_image_url AS profileImageUrl,
         e.weight
         FROM extrusion_templates et JOIN extrusions e ON e.id = et.extrusion_id
         WHERE et.id = @extrusionId
      """
    val fetch = DB.fetchOne(sql, Map("extrusionId" -> id.toString))(hydrateExtrusion) _

    db.withConnection(fetch)
  }

  def getExtrusions: Future[Seq[Extrusion]] = Future {
    val sql =
      """
         SELECT
         et.id,
         e.supplier_code AS mpn,
         et.name AS templateName,
         e.name AS name,
         et.image_url AS imageUrl,
         e.profile_image_url AS profileImageUrl,
         e.weight
         FROM extrusion_templates et JOIN extrusions e ON e.id = et.extrusion_id
      """
    val fetch = DB.fetchMany[Extrusion](sql, Map())(hydrateExtrusion) _

    db.withConnection(fetch)
  }

  private def hydrateExtrusion(rs: ResultSet): Extrusion =
    Extrusion(
      rs.getLong("id"),
      rs.getString("mpn"),
      rs.getString("name"),
      rs.getString("templateName"),
      rs.getString("imageUrl"),
      rs.getString("profileImageUrl")
    )
}
