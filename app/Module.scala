import com.google.inject.{AbstractModule, Provides}
import java.time.Clock

import cats.implicits._
import cats.effect.{ContextShift, IO, Resource}
import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor
import doobie.util.transactor.Transactor.Aux
import infra.DatabaseExecutionContext
import inventory.util.{DB, FileUploader}
import javax.sql.DataSource
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.{Configuration, Environment}
import shared.Types.Tx
import utils.Mailgun

import scala.concurrent.ExecutionContext

class Module(env: Environment, config: Configuration) extends AbstractModule {

  override def configure() = {
    // Use the system clock as the default implementation of Clock
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)

    val uploadPath = s"${env.rootPath}/${config.get[String]("paths.uploadDir")}"
    bind(classOf[FileUploader]).toInstance(new FileUploader(uploadPath))
  }

  @Provides
  def provideMailgun(ws: WSClient): Mailgun = {
    val mailgunDomain = config.get[String]("mailgun.domain")
    val mailgunApiKey = config.get[String]("mailgun.apiKey")

    new Mailgun(ws, mailgunDomain, mailgunApiKey)
  }

  @Provides
  def provideDB(db: Database)(implicit ec: DatabaseExecutionContext): DB = {
    new DB(db)
  }

  //  @Provides
  //  def provideTx(db: Database)(implicit dec: DatabaseExecutionContext): Aux[IO, Unit] = {
  //    val driver = config.get[String]("db.default.driver")
  //    val url = config.get[String]("db.default.url")
  //    val dbUser = config.get[String]("db.default.username")
  //    val dbPassword = config.get[String]("db.default.password")
  //
  //    implicit val cs = IO.contextShift(dec)
  //    Transactor.fromDriverManager[IO](driver, url, dbUser, dbPassword)
  //  }

  @Provides
  def provideTransactor(db: Database)(implicit ec: ExecutionContext): Tx = {
    implicit val cs = IO.contextShift(ec)
    println(db.dataSource.getClass.getName)

    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      te <- ExecutionContexts.cachedThreadPool[IO] // our transaction EC
    } yield Transactor.fromDataSource[IO](db.dataSource, ce, te) // Datasource is a hikari datasource
  }

//  @Provides
//  def provideHikariTransactor(db: Database)(implicit ec: ExecutionContext): Tx = {
//    val driver = config.get[String]("db.default.driver")
//    val url = config.get[String]("db.default.url")
//    val dbUser = config.get[String]("db.default.username")
//    val dbPassword = config.get[String]("db.default.password")
//    implicit val cs = IO.contextShift(ec)
//
//    val transactor: Resource[IO, HikariTransactor[IO]] =
//      for {
//        ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
//        te <- ExecutionContexts.cachedThreadPool[IO] // our transaction EC
//        xa <- HikariTransactor.newHikariTransactor[IO](
//          driver,
//          url,
//          dbUser,
//          dbPassword,
//          ce, // await connection here
//          te // execute JDBC operations here
//        )
//      } yield xa
//
//    transactor
//  }
}
