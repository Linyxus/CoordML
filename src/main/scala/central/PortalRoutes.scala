package central

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route

object PortalRoutes {
  val portalRoutes: Route =
    pathPrefix("portal") {
      get {
        getFromResourceDirectory("static")
      }
    }
}
