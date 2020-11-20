package central

import central.Functional.SessionCreated
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

object JsonFormats {

  import DefaultJsonProtocol._

  implicit val sysInfoJsonFormat: RootJsonFormat[SysInfo] = jsonFormat1(SysInfo)

  implicit val sessionCreatedJsonFormat: RootJsonFormat[SessionCreated] = jsonFormat1(SessionCreated)

  implicit val workerRegisteredFormat: RootJsonFormat[WorkerManager.WorkerRegistered] =
    jsonFormat1(WorkerManager.WorkerRegistered)

  implicit val gpuMemUsageFormat: RootJsonFormat[GpuMemUsage] = jsonFormat2(GpuMemUsage)
  implicit val gpuInfoFormat: RootJsonFormat[GpuInfo] = jsonFormat3(GpuInfo)
  implicit val workerInfoFormat: RootJsonFormat[WorkerInfo] = jsonFormat2(WorkerInfo)

  implicit val workersListFormat: RootJsonFormat[WorkerManager.WorkersList] =
    jsonFormat1(WorkerManager.WorkersList)

}
