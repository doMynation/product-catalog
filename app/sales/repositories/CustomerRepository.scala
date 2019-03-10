package sales.repositories

import java.sql.ResultSet
import cats.data.OptionT
import cats.effect.IO
import doobie._
import doobie.implicits._
import javax.inject.Inject
import infra.DatabaseExecutionContext
import inventory.util.{DB, SearchRequest, SearchResult}
import play.api.db.{Database, NamedDatabase}
import sales.entities.{Address, AddressType, Customer}
import shared.entities.PhoneNumber
import utils.SolariusDB
import scala.concurrent.Future

final class CustomerRepository @Inject()(@NamedDatabase("solarius") db: Database, solarius: SolariusDB)(implicit ec: DatabaseExecutionContext) {

  def getById(id: Long): IO[Option[Customer]] =
    solarius.run(Queries.getById(id))

  def getCustomerAddresses2(customerId: Long): IO[List[Address]] =
    solarius.run(Queries.getCustomerAddresses(customerId))

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

  private object Queries {
    def getCustomerAddresses(customerId: Long): ConnectionIO[List[Address]] = {
      val sql =
        sql"""
        SELECT
           ca.type_id,
           TRIM(CONCAT(ca.civic_number, " ", ca.street_name)) AS addressLine1,
           ca.unity AS addressLine2,
           ca.city,
           p.name AS stateName,
           c.name AS countryName,
           ca.postal_code AS zipCode
        FROM customer_addresses ca
           JOIN countries c ON c.id = ca.country_id
           JOIN provinces p ON p.id = ca.province_id
        WHERE customer_id = $customerId
        """

      sql
        .query[(Int, String, String, String, String, String, String)]
        .map(rec => Address(
          AddressType.fromId(rec._1).getOrElse(AddressType.BILLING),
          rec._2, rec._3, rec._4, "", rec._5, rec._6, rec._7
        ))
        .to[List]
    }

    def getById(customerId: Long): ConnectionIO[Option[Customer]] = {
      val sql =
        sql"""
            SELECT
              c.id, c.full_name,
              contacts.phone_number,
              contacts.phone_number2,
              contacts.phone_number3,
              contacts.phone_number3_extension,
              c.email
            FROM customers AS c
              JOIN contacts ON contacts.customer_id = c.id AND contacts.is_primary = 1 AND contacts.status = 1
            WHERE c.id = $customerId
            """

      // @todo: Move this to Customer.CustomerDB inner class
      val customerInfo = sql
        .query[(Long, String, String, String, String, String, String)]
        .map(rec => (
          rec._1,
          rec._2,
          List(
            PhoneNumber.format(rec._3),
            PhoneNumber.format(rec._4),
            PhoneNumber.format(rec._5, rec._6)
          ).filter(_.nonEmpty),
          List(rec._7).filter(_.nonEmpty),
        ))
        .option

      val program = for {
        d1 <- OptionT(customerInfo)
        addresses <- OptionT.liftF(getCustomerAddresses(d1._1))
      } yield Customer(
        id = d1._1,
        fullName = d1._2,
        phoneNumbers = d1._3,
        emails = d1._4,
        addresses = addresses
      )

      program.value
    }
  }
}
