package central

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import akka.util.Timeout
import spray.json._

class WorkerRoutes(workerManager: ActorRef[WorkerManager.Command])(implicit val system: ActorSystem[_]) {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._

  private implicit val timeout: Timeout = Timeout.create(system.settings.config.getDuration("coordml-central.routes.ask-timeout"))

  def workerRegister(request: WorkerRegisterRequest): Future[WorkerManager.WorkerRegistered] =
    workerManager.ask(WorkerManager.WorkerRegister(request, _))

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

  def workerReportResult(workerId: String, resultInfo: ResultInfo): Future[()] =
    workerManager.ask {
      WorkerManager.WorkerReportResult(workerId, resultInfo, _)
    }

  def workerRoutes: Route = cors() {
    pathPrefix("api") {
      pathPrefix("workers") {
        concat(
          pathPrefix("register") {
            post {
              entity(as[WorkerRegisterRequest]) { req =>
                onSuccess(workerRegister(req)) { resp =>
                  complete(resp)
                }
              }
            }
          },
          pathPrefix("list") {
            onSuccess(getWorkers) { resp =>
              complete(resp)
            }
          },
          pathPrefix("reportGpu") {
            post {
              entity(as[WorkerInfo]) { info =>
                onSuccess(workerReportGpu(info.workerId, info.gpuStatus)) {
                  complete(info)
                }
              }
            }
          },
          pathPrefix("fetchTasks") {
            parameters("workerId") { workerId =>
              onSuccess(workerTaskFetch(workerId)) { tasks =>
                complete(tasks)
              }
            }
          },
          pathPrefix("status") {
            onSuccess(getStatus) { x => complete(x) }
          },
          pathPrefix("reportResult") {
            post {
              parameters("workerId") { workerId =>
                entity(as[ResultInfo]) { res =>
                  onSuccess(workerReportResult(workerId, res)) {
                    complete(res)
                  }
                }
              }
            }
          }
        )
      }
    }
  }
}
