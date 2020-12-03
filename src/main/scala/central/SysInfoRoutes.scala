package central

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import central.SystemInfo._

// get system info actor
class SysInfoRoutes(sysInfo: ActorRef[SystemInfo.Command])(implicit val system: ActorSystem[_]) {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._

  private implicit val timeout: Timeout = Timeout.create(system.settings.config.getDuration("coordml-central.routes.ask-timeout"))

  def getSysInfo: Future[GetSysInfoResponse] =
    sysInfo ? GetSysInfo

  val sysInfoRoutes: Route =
    pathPrefix("api") {
      concat(
        path("sys_info") {
          concat(
            get {
              onSuccess(getSysInfo) { resp =>
                complete(resp.sysInfo)
              }
            }
          )
        }
      )
    }
}
