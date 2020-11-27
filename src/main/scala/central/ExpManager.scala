package central

import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.receptionist.Receptionist
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.persistence.typed.PersistenceId
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import central.WorkerManager.WorkerTaskDispatched
import spray.json.JsValue
import akka.util.Timeout

import monocle.macros.Lenses
import monocle.function.Index._
import monocle.Traversal
import cats.implicits._

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

case class CreateExpRequest(title: String, author: String, config: JsValue, resolverPath: String,
                            envPath: String, resultParse: String, resultView: ResultView)

object ExpManager {

  sealed trait Command

  final case class ExpCreate(request: CreateExpRequest, replyTo: ActorRef[Either[String, ExpCreated]]) extends Command

  final case class ExpCreated(expId: String)

  final case class TaskDispatchedWrapper(resp: WorkerTaskDispatched) extends Command

  final case class WorkerReportResult(resultInfo: ResultInfo) extends Command

  final case class GetExpOverview(expId: String, replyTo: ActorRef[Option[ExpOverviewResponse]]) extends Command

  final case class ExpOverviewResponse(expId: String, title: String, envPath: String, progress: Double)

  sealed trait Event

  final case class AddExpInstance(expInstance: ExpInstance) extends Event with JacksonEvt

  final case class UpdateExpTaskInstances(resp: WorkerTaskDispatched) extends Event with JacksonEvt

  final case class UpdateResultInfo(resultInfo: ResultInfo) extends Event with JacksonEvt

  @Lenses
  final case class State(expInstances: Map[String, ExpInstance])

  val ExpManagerKey: ServiceKey[Command] = ServiceKey[ExpManager.Command]("exp-manager")

  def apply(workerManager: ActorRef[WorkerManager.Command]): Behavior[Command] = Behaviors.setup[Command] { context =>
    context.system.receptionist ! Receptionist.Register(ExpManagerKey, context.self)

    EventSourcedBehavior[Command, Event, State](
      persistenceId = PersistenceId.ofUniqueId("exp-manager"),
      emptyState = State(Map.empty),
      commandHandler = (_, cmd) => cmd match {
        case GetExpOverview(expId, replyTo) =>
          Effect.none.thenReply(replyTo) { state =>
            val f = State.expInstances ^|-? index(expId) getOption state
            f.map { e =>
              val tasks = ExpInstance.tasksOf(e)
              ExpOverviewResponse(
                expId = e.expId,
                title = e.blueprint.title,
                envPath = e.blueprint.envPath,
                progress = tasks.count { t =>
                  t.status match {
                    case _: TaskStatusTodo => false
                    case TaskStatusDone(_) => true
                  }
                } .toDouble / tasks.length.toDouble
              )
            }
          }
        case ExpCreate(request, replyTo) =>
          ExpBlueprint.fromRequest(request) match {
            case Left(value) => Effect.none.thenReply(replyTo) { _ => Left(value) }
            case Right(value) =>
              val expInst = ExpInstance fromBluePrint value
              implicit val timeout: Timeout =
                Timeout.create(context.system.settings.config.getDuration("coordml-central.routes.ask-timeout"))
              Effect.persist(AddExpInstance(expInst)).thenRun { _ =>
                replyTo ! Right {
                  ExpCreated {
                    expInst.expId
                  }
                }
                context.ask(workerManager, ref => WorkerManager.WorkerTaskDispatch(expInst, ref)) {
                  case Success(value) => TaskDispatchedWrapper(value)
                  case Failure(_) => TaskDispatchedWrapper(WorkerTaskDispatched(expInst.expId, Map.empty))
                }
              }
          }
        case TaskDispatchedWrapper(resp) => Effect.persist {
          UpdateExpTaskInstances(resp)
        }
        case WorkerReportResult(resultInfo) => Effect.persist {
          UpdateResultInfo(resultInfo)
        }
      },
      eventHandler = (state, evt) => evt match {
        case AddExpInstance(expInstance) => State(state.expInstances.updated(expInstance.expId, expInstance))
        case UpdateExpTaskInstances(resp) =>
          val f =
            State.expInstances ^|-? index(resp.expId) ^|-> ExpInstance.workerSchedule set resp.schedule
          f(state)
        case UpdateResultInfo(resultInfo) =>
          val f =
            State.expInstances ^|-? index(resultInfo.expId) ^|->
              ExpInstance.taskGraphs ^|-? index(resultInfo.graphId) ^|->
              TaskGraph.nodes ^|->>
              Traversal.fromTraverse[List, TaskInstance] modify { task =>
              if (task.taskId == resultInfo.taskId) {
                TaskInstance.status.set(TaskStatusDone(resultInfo.results))(task)
              } else task
            }
          f(state)
      }
    )
  }
}
