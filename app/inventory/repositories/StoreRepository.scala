package accounting.repositories

import java.sql.ResultSet
import javax.inject.Inject
import infrastructure.DatabaseExecutionContext
import inventory.entities.Store
import inventory.util.DB
import play.api.db.Database
import utils.StoreId
import scala.concurrent.Future

final class StoreRepository @Inject()(db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(id: StoreId): Future[Option[Store]] = Future {
    db.withConnection { conn =>
      val sql = "SELECT inv_stores.* FROM inv_stores WHERE id = @storeId"
      val params = Map("apiKey" -> id.value.toString)

      DB.fetchOne(sql, params)(hydrateStore)(conn)
    }
  }

  def get(apiKey: String): Future[Option[Store]] = Future {
    db.withConnection { conn =>
      val sql = "SELECT inv_stores.* FROM inv_stores WHERE api_key = @apiKey"
      val params = Map("apiKey" -> apiKey)

      DB.fetchOne(sql, params)(hydrateStore)(conn)
    }
  }

  private def hydrateStore(rs: ResultSet): Store = {
    Store(
      rs.getLong("id"),
      rs.getString("name"),
      rs.getTimestamp("creation_date").toLocalDateTime
    )
  }
}
