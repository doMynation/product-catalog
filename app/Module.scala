import com.google.inject.AbstractModule
import java.time.Clock
import inventory.util.FileUploader
import play.api.{Configuration, Environment}

/**
  * This class is a Guice module that tells Guice how to bind several
  * different types. This Guice module is created when the Play
  * application starts.
  *
  * Play will automatically use any class called `Module` that is in
  * the root package. You can create modules in other locations by
  * adding `play.modules.enabled` settings to the `application.conf`
  * configuration file.
  */
class Module(env: Environment, config: Configuration) extends AbstractModule {

  override def configure() = {
    // Use the system clock as the default implementation of Clock
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)

    val uploadPath = s"${env.rootPath}/${config.get[String]("paths.uploadDir")}"
    bind(classOf[FileUploader]).toInstance(new FileUploader(uploadPath))
  }
}
