package central

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import central.Functional._
import spray.json._

class FunctionalRoutes(functional: ActorRef[Functional.Command])(implicit val system: ActorSystem[_]) {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._

  private implicit val timeout: Timeout = Timeout.create(system.settings.config.getDuration("coordml-central.routes.ask-timeout"))

  def evalSircleExpr(source: String): Future[EvalSircleExprResponse] =
    functional.ask(EvalSircleExpr(source, _))

  def jsonToSircleValue(jsValue: JsValue): Future[JsonToSircleValueResponse] =
    functional.ask(JsonToSircleValue(jsValue, _))

  def createSession: Future[SessionCreated] =
    functional.ask(CreateSession)

  def sessEvalSircleExpr(source: String, sessId: Int): Future[EvalSircleExprResponse] =
    functional.ask(SessEvalSircleExpr(source, sessId, _))


  val functionalRoutes: Route =
    pathPrefix("api") {
      concat(
        pathPrefix("sircle") {
          concat(
            pathPrefix("eval") {
              parameters("source") { source =>
                rejectEmptyResponse {
                  onSuccess(evalSircleExpr(source)) { resp =>
                    complete(resp.maybeResult)
                  }
                }
              }
            },
            pathPrefix("convert") {
              post {
                entity(as[JsValue]) { jsValue =>
                  rejectEmptyResponse {
                    onSuccess(jsonToSircleValue(jsValue)) { resp =>
                      complete(resp.maybeResult)
                    }
                  }
                }
              }
            },
            pathPrefix("session") {
              concat(
                pathPrefix("create") {
                  onSuccess(createSession) { resp =>
                    complete(resp)
                  }
                },
                pathPrefix("eval") {
                  parameter("source", "sess_id") { (source, sessId) =>
                    rejectEmptyResponse {
                      onSuccess(sessEvalSircleExpr(source, sessId.toInt)) { resp =>
                        complete(resp.maybeResult)
                      }
                    }
                  }
                }
              )
            }
          )
        }
      )
    }
}
