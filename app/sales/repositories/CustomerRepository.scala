package sales.repositories

import cats.data.OptionT
import cats.effect.IO
import doobie._
import doobie.implicits._
import javax.inject.Inject
import sales.entities.{Address, AddressType, Customer}
import shared.entities.PhoneNumber
import utils.SolariusDB

final class CustomerRepository @Inject()(solarius: SolariusDB) {

  def getById(id: Long): IO[Option[Customer]] =
    solarius.run(Queries.getById(id))

  def getCustomerAddresses(customerId: Long): IO[List[Address]] =
    solarius.run(Queries.getCustomerAddresses(customerId))

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
          rec._2, rec._3, "", rec._4, rec._5, rec._6, rec._7
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
