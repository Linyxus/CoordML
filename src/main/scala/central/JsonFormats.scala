package central

import central.Functional.SessionCreated
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

object JsonFormats  {
  import DefaultJsonProtocol._

  implicit val sysInfoJsonFormat: RootJsonFormat[SysInfo] = jsonFormat1(SysInfo)

  implicit val sessionCreatedJsonFormat: RootJsonFormat[SessionCreated] = jsonFormat1(SessionCreated)
}
