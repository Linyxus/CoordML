package central

import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.receptionist.Receptionist
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior, RetentionCriteria}
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

final case class ResultTable(columns: List[String], results: List[List[String]])

final case class RenderedTask(taskId: String, executable: String, status: String, args: String, tags: String)

object ExpManager {

  sealed trait Command

  final case class ExpCreate(request: CreateExpRequest, replyTo: ActorRef[Either[String, ExpCreated]]) extends Command

  final case class ExpCreated(expId: String)

  final case class TaskDispatchedWrapper(resp: WorkerTaskDispatched) extends Command

  final case class WorkerReportResult(resultInfo: ResultInfo) extends Command

  final case class GetExpOverview(expId: String, replyTo: ActorRef[Option[ExpOverviewResponse]]) extends Command

  final case class ListExpOverview(replyTo: ActorRef[ExpOverviewListing]) extends Command

  final case class ExpOverviewResponse(expId: String, title: String, envPath: String, progress: Double)

  final case class ExpOverviewListing(experiments: List[ExpOverviewResponse])

  final case class ListExpResults(expId: String, replyTo: ActorRef[Option[ResultTable]]) extends Command

  final case class GetExpView(expId: String, replyTo: ActorRef[Option[ResultTable]]) extends Command

  final case class ListRenderedTasks(expId: String, replyTo: ActorRef[Option[RenderedTaskListing]]) extends Command

  final case class RenderedTaskListing(tasks: List[RenderedTask])

  sealed trait Event

  final case class AddExpInstance(expInstance: ExpInstance) extends Event with JacksonEvt

  final case class UpdateExpTaskInstances(resp: WorkerTaskDispatched) extends Event with JacksonEvt

  final case class UpdateResultInfo(resultInfo: ResultInfo) extends Event with JacksonEvt

  @Lenses
  final case class State(expInstances: Map[String, ExpInstance]) extends JacksonEvt

  def buildRelationalTable(columnKeys: List[String], tuples: List[(Map[String, String], Map[String, Double])]): ResultTable = {
    // get all keys in results
    val resultKeys: List[String] = tuples.flatMap { case (_, row) => row.keys }.distinct
    // get all meta key existence
    val metaKeys: List[List[String]] = (tuples flatMap { case (meta, _) =>
      Tools.gatherOption {
        columnKeys map { key => meta.get(key) }
      }
    }).distinct
    // aggregate results for each meta key
    val results = metaKeys.map { metaKey =>
      tuples.filter { case (meta, _) =>
        Tools.gatherOption {
          columnKeys map { key => meta.get(key) }
        } contains metaKey
      }.map { case (_, row) => row }
    }.map { rows =>
      resultKeys.map { key =>
        "%.6f".format {
          Tools.maybeAverage {
            rows.map { res => res get key }
          }
        }
      }
    }
    ResultTable(
      columns = columnKeys ++ resultKeys,
      results = metaKeys zip results map { case (l, r) => l ++ r }
    )
  }

  def renderNamedPair(pair: (String, String)): String = pair match {
    case (l, r) => s"$l = $r"
  }

  def renderTask(taskInstance: TaskInstance): RenderedTask =
    RenderedTask(
      taskId = taskInstance.taskId,
      executable = taskInstance.executable,
      status = taskInstance.status match {
        case _: TaskStatusTodo => "Running"
        case TaskStatusDone(_) => "Done"
      },
      args = taskInstance.args.toList.map(renderNamedPair) mkString "<br/>",
      tags = taskInstance.meta.toList.map(renderNamedPair) mkString "<br/>"
    )

  val ExpManagerKey: ServiceKey[Command] = ServiceKey[ExpManager.Command]("exp-manager")

  def apply(workerManager: ActorRef[WorkerManager.Command]): Behavior[Command] = Behaviors.setup[Command] { context =>
    context.system.receptionist ! Receptionist.Register(ExpManagerKey, context.self)

    EventSourcedBehavior[Command, Event, State](
      persistenceId = PersistenceId.ofUniqueId("exp-manager"),
      emptyState = State(Map.empty),
      commandHandler = (_, cmd) => cmd match {
        case ListRenderedTasks(expId, replyTo) =>
          Effect.none.thenReply(replyTo) { state =>
            State.expInstances ^|-? index(expId) getOption state map { exp =>
              RenderedTaskListing { exp.taskGraphs.values.flatMap { g => g.nodes.map(renderTask) }.toList }
            }
          }
        case GetExpView(expId, replyTo) =>
          Effect.none.thenReply(replyTo) { state =>
            State.expInstances ^|-? index(expId) getOption state map { exp =>
              if (exp.blueprint.resultView.rowKey.isEmpty) {
                val tuples = exp.taskGraphs.values.flatMap { g =>
                  g.nodes.flatMap { t =>
                    t.status match {
                      case _: TaskStatusTodo => None
                      case TaskStatusDone(results) => Some(t.meta -> results)
                    }
                  }
                }
                buildRelationalTable(exp.blueprint.resultView.columnKey, tuples.toList)
              } else ???
            }
          }
        case ListExpResults(expId, replyTo) =>
          Effect.none.thenReply(replyTo) { state =>
            State.expInstances ^|-? index(expId) ^|-> ExpInstance.taskGraphs getOption state map { taskGraphs =>
              val tasks = taskGraphs.values flatMap { g => g.nodes }
              val metaKeys: List[String] = tasks.flatMap { t => t.meta.keys }.toList.distinct
              val resultKeys: List[String] = tasks.flatMap { t =>
                t.status match {
                  case _: TaskStatusTodo => Nil
                  case TaskStatusDone(results) => results.keys
                }
              }.toList.distinct
              val rows: List[List[String]] = tasks.map { t =>
                t.status match {
                  case _: TaskStatusTodo => Nil
                  case TaskStatusDone(results) =>
                    val meta: List[String] = metaKeys.map { key => t.meta.getOrElse(key, "---") }
                    val res: List[String] = resultKeys.map { key => results get key map { x => "%.6f".format(x) } getOrElse "---" }
                    t.taskId :: meta ++ res
                }
              }.toList
              ResultTable(
                columns = "ID" :: metaKeys ++ resultKeys,
                results = rows
              )
            }
          }
        case ListExpOverview(replyTo) =>
          Effect.none.thenReply(replyTo) { state =>
            ExpOverviewListing {
              state.expInstances.map { case (_, e) =>
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
                  }.toDouble / tasks.length.toDouble
                )
              }.toList
            }
          }
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
                }.toDouble / tasks.length.toDouble
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
      .snapshotWhen { case (_, _, _) => false }
      .withRetention(RetentionCriteria.snapshotEvery(
        numberOfEvents = 50,
        keepNSnapshots = 2
      ))
  }
}
