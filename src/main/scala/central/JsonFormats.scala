package central

import spray.json.DefaultJsonProtocol

object JsonFormats  {
  import DefaultJsonProtocol._

  implicit val sysInfoJsonFormat = jsonFormat1(SysInfo)
}
