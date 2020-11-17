package central

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

object JsonFormats  {
  import DefaultJsonProtocol._

  implicit val sysInfoJsonFormat: RootJsonFormat[SysInfo] = jsonFormat1(SysInfo)
}
