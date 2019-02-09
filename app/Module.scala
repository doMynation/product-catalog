import com.google.inject.{AbstractModule, Provides}
import java.time.Clock
import inventory.util.FileUploader
import play.api.libs.ws.WSClient
import play.api.{Configuration, Environment}
import utils.Mailgun

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
}
