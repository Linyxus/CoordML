package central

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives.concat

import scala.util.{Failure, Success}

object CentralApp {
  private def startHttpServer(routes: Route)(implicit system: ActorSystem[_]): Unit = {
    import system.executionContext

    val futureBinding = Http().newServerAt("0.0.0.0", 8888).bind(routes)
    futureBinding.onComplete {
      case Success(binding) =>
        val address = binding.localAddress
        system.log.info("Server online at http://{}:{}/", address.getHostString, address.getPort)
      case Failure(ex) =>
        system.log.error("Failed to bind HTTP endpoint, terminating system", ex)
        system.terminate()
    }
  }

  def main(args: Array[String]): Unit = {
    val rootBehavior = Behaviors.setup[Nothing] { context =>
      val systemInfoActor = context.spawn(SystemInfo(), "SystemInfoActor")
      context.watch(systemInfoActor)

      val functionalActor = context.spawn(Functional(), "FunctionalActor")
      context.watch(functionalActor)

      val statefulActor = context.spawn(Stateful(), "StatefulActor")
      context.watch(statefulActor)

      val workerManagerActor = context.spawn(WorkerManager(), "WorkerManagerActor")
      context.watch(workerManagerActor)

      val expManagerActor = context.spawn(ExpManager(workerManagerActor), "ExpManagerActor")
      context.watch(expManagerActor)

      val routes = concat(
        new SysInfoRoutes(systemInfoActor)(context.system).sysInfoRoutes,
        new FunctionalRoutes(functionalActor, statefulActor)(context.system).functionalRoutes,
        new WorkerRoutes(workerManagerActor)(context.system).workerRoutes,
        new ExpRoutes(expManagerActor)(context.system).expRoutes,
        PortalRoutes.portalRoutes
      )
      startHttpServer(routes)(context.system)

      Behaviors.empty
    }
    val system = ActorSystem[Nothing](rootBehavior, "HelloAkkaHttpServer")
  }
}
