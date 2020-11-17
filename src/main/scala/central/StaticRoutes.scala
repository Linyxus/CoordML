package central

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

object StaticRoutes {
  val staticRoutes: Route = getFromResourceDirectory("static")
}
