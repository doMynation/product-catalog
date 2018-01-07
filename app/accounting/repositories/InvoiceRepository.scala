package accounting.repositories

import java.sql.ResultSet
import javax.inject.Inject

import accounting.entities.Invoice
import inventory.util.SearchRequest
import play.api.db.{Database, NamedDatabase}
import shared.Repository

import scala.concurrent.{ExecutionContext, Future}

final class InvoiceRepository @Inject()(@NamedDatabase("solarius") db: Database)(implicit ec: ExecutionContext) extends Repository[Invoice] {
  override def get(id: Long): Option[Invoice] = db.withConnection { conn =>
    val stmt = conn.prepareStatement(
      s"""
          SELECT *
          FROM s_invoices i
          WHERE i.id = ?
       """.stripMargin)

    stmt.setLong(1, id)

    val rs = stmt.executeQuery

    if (rs.next) Some(hydrateInvoice(rs))
    else None
  }

  override def search(sr: SearchRequest, inclusions: Seq[String]): Future[Seq[Invoice]] = Future {
    db.withConnection { conn =>
      List(
        Invoice(Some(4843), "some name"),
        Invoice(Some(222), "Bob \"Graton\""),
        Invoice(Some(222), "V94304-1")
      )
    }
  }

  private def hydrateInvoice(rs: ResultSet): Invoice = {
    val ts = rs.getTimestamp("modification_date")
    val updatedAt = if (rs.wasNull()) None else Option(ts.toLocalDateTime)

    Invoice(
      Some(rs.getLong("id")),
      rs.getString("sale_id"),
      rs.getTimestamp("creation_date").toLocalDateTime,
      updatedAt
    )
  }
}
