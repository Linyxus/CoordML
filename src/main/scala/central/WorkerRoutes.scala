package central

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import spray.json._

class WorkerRoutes(workerManager: ActorRef[WorkerManager.Command])(implicit val system: ActorSystem[_]) {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._

  private implicit val timeout: Timeout = Timeout.create(system.settings.config.getDuration("coordml-central.routes.ask-timeout"))

  def workerRegister: Future[WorkerManager.WorkerRegistered] =
    workerManager ? WorkerManager.WorkerRegister

  def getWorkers: Future[WorkerManager.WorkersList] =
    workerManager ? WorkerManager.GetWorkers

  def workerReportGpu(workerId: String, gpuInfo: List[GpuInfo]): Future[()] =
    workerManager.ask {
      WorkerManager.WorkerReportGpu(workerId, gpuInfo, _)
    }

  def workerTaskFetch(workerId: String): Future[WorkerManager.WorkerTaskFetched] =
    workerManager.ask {
      WorkerManager.WorkerTaskFetch(workerId, _)
    }

  def getStatus: Future[WorkerManager.StatusResponse] =
    workerManager ? WorkerManager.GetStatus

  def workerRoutes: Route =
    pathPrefix("api") {
      pathPrefix("workers") {
        concat(
          pathPrefix("register") {
            onSuccess(workerRegister) { resp =>
              complete(resp)
            }
          },
          pathPrefix("list") {
            onSuccess(getWorkers) { resp =>
              complete(resp)
            }
          },
          pathPrefix("reportGpu") {
            entity(as[WorkerInfo]) { info =>
              onSuccess(workerReportGpu(info.workerId, info.gpuStatus)) {
                complete("okay")
              }
            }
          },
          pathPrefix("fetchTasks") {
            entity(as[String]) { workerId =>
              onSuccess(workerTaskFetch(workerId)) { tasks =>
                complete(tasks)
              }
            }
          },
          pathPrefix("status") {
            onSuccess(getStatus) { x => complete(x) }
          }
        )
      }
    }
}
