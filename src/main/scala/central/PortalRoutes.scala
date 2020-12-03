package central

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route

// static route for serving portal
object PortalRoutes {
  val portalRoutes: Route =
    pathPrefix("") {
      get {
        getFromResourceDirectory("portal")
      }
    }
}
