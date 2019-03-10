import java.time.Clock

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.google.inject.{AbstractModule, Provides}
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor
import inventory.util.FileUploader
import play.api.db.{Database, NamedDatabase}
import play.api.libs.ws.WSClient
import play.api.{Configuration, Environment}
import shared.Types.{Tx}
import utils.{Mailgun, SolariusDB}

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

  //  @Provides
  //  def provideTx(db: Database)(implicit dec: DatabaseExecutionContext): Aux[IO, Unit] = {
  //    val driver = config.get[String]("db.default.driver")
  //    val url = config.get[String]("db.default.url")
  //    val dbUser = config.get[String]("db.default.username")
  //    val dbPassworde= config.get[String]("db.default.password")
  //
  //    implicit val cs = IO.contextShift(dec)
  //    Transactor.fromDriverManager[IO](driver, url, dbUser, dbPassword)
  //  }

  @Provides
  def provideContextShift()(implicit ec: ExecutionContext): ContextShift[IO] = {
    IO.contextShift(ec)
  }

  @Provides
  def provideTransactor(db: Database)(implicit cs: ContextShift[IO]): Tx = {
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // The EC for awaiting connections
      te <- ExecutionContexts.cachedThreadPool[IO] // The EC for transactions
    } yield Transactor.fromDataSource[IO](db.dataSource, ce, te) // Datasource is a hikari datasource
  }

  @Provides
  def provideSolariusTransactor(@NamedDatabase("solarius") db: Database)(implicit cs: ContextShift[IO]): SolariusDB = {
    val resource = for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      te <- ExecutionContexts.cachedThreadPool[IO] // our transaction EC
    } yield Transactor.fromDataSource[IO](db.dataSource, ce, te) // Datasource is a hikari datasource

    new SolariusDB(resource)
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
