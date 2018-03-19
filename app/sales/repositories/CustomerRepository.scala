package accounting.repositories

import java.sql.ResultSet
import javax.inject.Inject

import infrastructure.DatabaseExecutionContext
import inventory.util.{DatabaseHelper, SearchRequest, SearchResult}
import play.api.db.{Database, NamedDatabase}
import sales.entities.{Address, AddressType, Customer}
import shared.PhoneNumber

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class CustomerRepository @Inject()(@NamedDatabase("solarius") db: Database)(implicit ec: DatabaseExecutionContext) {

  def get(id: Long): Future[Option[Customer]] = Future {
    db.withConnection { conn =>
      val sql =
        """
          SELECT
            c.*,
            contacts.gender_id,
            contacts.phone_number,
            contacts.phone_number2,
            contacts.phone_number3,
            contacts.phone_number3_extension,
            author.username AS authorUsername,
            author.full_name AS authorName,
            store.label AS storeName
          FROM customers AS c
            JOIN contacts ON contacts.customer_id = c.id AND contacts.is_primary = 1 AND contacts.status = 1
            JOIN users author ON author.id = c.user_id
            JOIN branches store ON store.id = c.branch_id
          WHERE c.id = @customerId;
        """

      val params = Map(
        "customerId" -> id.toString
      )

      val customer = DatabaseHelper.fetchOne[Customer](sql, params)(hydrateCustomer)(conn)

      customer.map(cus => cus.copy(addresses = getCustomerAddresses(cus.id)))
    }
  }

  def getCustomerAddresses(customerId: Long): Seq[Address] = db.withConnection { conn =>
    val sql =
      """
        SELECT
           ca.*,
           TRIM(CONCAT(ca.civic_number, " ", ca.street_name)) AS addressLine1,
           ca.unity AS addressLine2,
           c.name AS countryName,
           c.iso_code_2 AS countryCode,
           p.name AS stateName,
           ca.postal_code AS zipCode
         FROM customer_addresses ca
           JOIN countries c ON c.id = ca.country_id
           JOIN provinces p ON p.id = ca.province_id
         WHERE customer_id = @customerId;
        """

    val params = Map(
      "customerId" -> customerId.toString
    )

    DatabaseHelper.fetchMany[Address](sql, params)(hydrateAddress)(conn)
  }

  def search(sr: SearchRequest, inclusions: Seq[String]): Future[SearchResult[Customer]] = Future {
    db.withConnection { conn =>
      val wheres = new ListBuffer[String]()
      val havings = new ListBuffer[String]()
      val joins = new ListBuffer[String]()
      var params: Map[String, String] = Map()

      wheres += "1 = 1"
      havings += "1 = 1"

      // `name` filter
      sr.filters.get("name").foreach(value => {
        wheres += "i.sale_id LIKE @name"
        params = params + ("name" -> s"%$value%")
      })

      // `storeId` filter
      sr.filters.get("storeId").foreach(value => {
        wheres += "i.branch_id = @storeId"
        params = params + ("storeId" -> value.toString)
      })

      // `customerName` filter
      sr.filters.get("customerName").foreach(value => {
        wheres += "c.full_name LIKE @customerName"
        params = params + ("customerName" -> s"%$value%")
      })

      // `orderName` filter
      sr.filters.get("orderName").foreach(value => {
        wheres += "o.sale_id LIKE @orderName"
        params = params + ("orderName" -> s"%$value%")
      })

      //      // `status` filter
      //      sr.filters.get("status").flatMap(InvoiceStatus.fromString(_)).foreach(statusId => {
      //        wheres += "i.status = @statusId"
      //        params = params + ("statusId" -> statusId.toString)
      //      })

      val allowedSortFields = Map(
        "id" -> "i.id",
        "orderId" -> "i.order_id",
        "orderName" -> "o.sale_id",
        "name" -> "i.sale_id",
        "type" -> "i.type_id",
        "currency" -> "i.currency_id",
        "subtotal" -> "i.sub_total",
        "total" -> "i.total",
        "balance" -> "i.balance",
        "paidAmount" -> "i.deposit",
        "createdAt" -> "i.creation_date",
        "updatedAt" -> "i.modification_date",
        "status" -> "i.status",
        "customerName" -> "c.full_name",
      )

      val sortField = sr.sortField.flatMap(allowedSortFields.get(_)).getOrElse("i.creation_date")
      val countSql =
        s"""
              SELECT COUNT(*)
              FROM s_invoices i
              JOIN customers c ON c.id = i.customer_id
              LEFT JOIN s_orders o ON o.id = i.order_id
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
        """

      val totalCount = DatabaseHelper.fetchColumn[Int](countSql, params)(conn)

      val fetchSql =
        s"""
              SELECT
                i.*,
                c.*,
                o.sale_id AS orderName
              FROM s_invoices i
              JOIN customers c ON c.id = i.customer_id
              LEFT JOIN s_orders o ON o.id = i.order_id
              ${joins.mkString(" ")}
              WHERE ${wheres.mkString(" AND ")}
              HAVING ${havings.mkString(" AND ")}
              ORDER BY $sortField ${sr.sortOrder}
              ${sr.limit.map(lim => s"LIMIT ${sr.offset}, $lim").getOrElse("LIMIT 100")}
        """

      val results = DatabaseHelper.fetchMany(fetchSql, params)(hydrateCustomer)(conn)

      SearchResult(results, totalCount.get)
    }
  }

  private def hydrateCustomer(rs: ResultSet): Customer = {
    val phoneNumbers = List(
      PhoneNumber.format(rs.getString("phone_number")),
      PhoneNumber.format(rs.getString("phone_number2")),
      PhoneNumber.format(rs.getString("phone_number3"), rs.getString("phone_number3_extension"))
    ).filter(_ != "")

    Customer(
      id = rs.getLong("c.id"),
      fullName = rs.getString("c.full_name"),
      emails = List(rs.getString("c.email")),
      phoneNumbers
    )
  }

  private def hydrateAddress(rs: ResultSet): Address = {
    val addressType = AddressType.fromId(rs.getInt("type_id")).getOrElse(AddressType.BILLING)

    Address(
      addressType = addressType,
      addressLine1 = rs.getString("addressLine1"),
      addressLine2 = rs.getString("addressLine2"),
      city = rs.getString("city"),
      state = rs.getString("stateName"),
      country = rs.getString("countryName"),
      zipCode = rs.getString("zipCode"),
    )
  }
}
