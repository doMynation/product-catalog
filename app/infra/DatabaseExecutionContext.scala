package infra

import javax.inject.Inject
import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext

class DatabaseExecutionContext @Inject()(system: ActorSystem) extends CustomExecutionContext(system, "database-context")
