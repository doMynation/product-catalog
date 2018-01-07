package accounting.repositories

import java.sql.ResultSet
import javax.inject.Inject

import inventory.entities.Store
import inventory.util.SearchRequest
import play.api.db.Database
import shared.Repository

import scala.concurrent.ExecutionContext

final class StoreRepository @Inject()(db: Database)(implicit ec: ExecutionContext) extends Repository[Store] {
  override def get(id: Long): Option[Store] = db.withConnection { conn =>
    val stmt = conn.prepareStatement("SELECT * FROM inv_stores s WHERE i.id = ?")
    stmt.setLong(1, id)

    val rs = stmt.executeQuery

    if (rs.next) Some(hydrateStore(rs))
    else None
  }

  def getByApiKey(apiKey: String, ip: String): Option[Store] = db.withConnection { conn =>
    val stmt = conn.prepareStatement("SELECT inv_stores.* FROM inv_stores JOIN store_ips ON store_ips.store_id = inv_stores.id AND store_ips.ip = ? WHERE api_key = ?")
    stmt.setString(1, ip)
    stmt.setString(2, apiKey)

    val rs = stmt.executeQuery

    if (rs.next) Some(hydrateStore(rs))
    else None
  }

  private def hydrateStore(rs: ResultSet): Store = {
    Store(
      Some(rs.getLong("id")),
      rs.getString("name"),
      rs.getTimestamp("creation_date").toLocalDateTime
    )
  }

  override def search(sr: SearchRequest, inclusions: Seq[String]) = ???
}
