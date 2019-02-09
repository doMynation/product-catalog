package sales.repositories

import java.sql.ResultSet
import javax.inject.Inject

import infra.DatabaseExecutionContext
import inventory.util.{DB, SearchRequest, SearchResult}
import play.api.Logger
import play.api.db.{Database, NamedDatabase}
import sales.entities.{Address, AddressType, Customer}
import shared.entities.PhoneNumber

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

      val customer = DB.fetchOne[Customer](sql, params)(hydrateCustomer)(conn)

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

    DB.fetchMany[Address](sql, params)(hydrateAddress)(conn)
  }

  def search(sr: SearchRequest, inclusions: Seq[String]): Future[SearchResult[Customer]] = ???

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
