package central

import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.persistence.typed.PersistenceId
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.receptionist.Receptionist
import java.util.UUID.randomUUID

import akka.persistence.typed.scaladsl.RetentionCriteria
import monocle.macros.Lenses
import monocle.function.At._
import monocle.function.Index._

final case class GpuMemUsage(used: Double, capacity: Double)

final case class GpuInfo(name: String, memUsage: GpuMemUsage, load: Double)

final case class WorkerRegisterRequest(name: String)

@Lenses
final case class WorkerInfo(workerId: String, name: String, gpuStatus: List[GpuInfo], pendingTasks: List[RunnableGraph])

final case class ResultInfo(expId: String, graphId: String, taskId: String, results: Map[String, Double])

// worker management actor
object WorkerManager {

  sealed trait Command

  final case class WorkerRegister(request: WorkerRegisterRequest, replyTo: ActorRef[WorkerRegistered]) extends Command

  final case class WorkerRegistered(workerId: String)

  final case class GetWorkers(replyTo: ActorRef[WorkersList]) extends Command

  final case class WorkersList(workers: List[WorkerInfo])

  final case class WorkerReportGpu(workerId: String, gpuInfo: List[GpuInfo], replyTo: ActorRef[Unit]) extends Command

  final case class WorkerTaskDispatch(expInstance: ExpInstance,
                                      replyTo: ActorRef[WorkerTaskDispatched]) extends Command

  final case class WorkerTaskDispatched(expId: String, schedule: Map[String, String])

  final case class WorkerTaskFetch(workerId: String, replyTo: ActorRef[WorkerTaskFetched]) extends Command

  final case class WorkerTaskFetched(tasks: List[RunnableGraph])

  final case class WorkerReportResult(workerId: String, resultInfo: ResultInfo, replyTo: ActorRef[Unit]) extends Command

  final case class ListingResponse(listing: Receptionist.Listing) extends Command

  final case class GetStatus(replyTo: ActorRef[StatusResponse]) extends Command

  final case class StatusResponse(workerNum: Int, expManagerConnected: Boolean)

  sealed trait Event

  final case class AddWorker(workerId: String, name: String) extends Event with JacksonEvt

  final case class UpdateWorkerGpu(workerId: String, gpuInfo: List[GpuInfo]) extends Event with JacksonEvt

  final case class AppendWorkerTask(workerId: String, runnableGraph: RunnableGraph) extends Event with JacksonEvt

  final case class CleanupWorkerTask(workerId: String) extends Event with JacksonEvt

  final case class FindExpManager(actorRef: ActorRef[ExpManager.Command]) extends Event with JacksonEvt

  @Lenses
  final case class State(workers: Map[String, WorkerInfo], expManager: Option[ActorRef[ExpManager.Command]]) extends JacksonEvt

  def apply(): Behavior[Command] = Behaviors.setup[Command] { context =>
    val listingResponseAdapter = context.messageAdapter[Receptionist.Listing](ListingResponse)
    context.system.receptionist ! Receptionist.Subscribe(ExpManager.ExpManagerKey, listingResponseAdapter)

    EventSourcedBehavior[Command, Event, State](
      persistenceId = PersistenceId.ofUniqueId("worker-manager"),
      emptyState = State(Map.empty, None),
      commandHandler = (state, cmd) => cmd match {
        case GetStatus(replyTo) =>
          Effect.none.thenReply(replyTo) { state =>
            StatusResponse(state.workers.toList.length, state.expManager.isDefined)
          }
        case ListingResponse(ExpManager.ExpManagerKey.Listing(listings)) =>
          if (listings.nonEmpty)
            Effect.persist(FindExpManager(listings.head))
          else
            Effect.none
        case WorkerRegister(req, replyTo) =>
          val workerId = randomUUID().toString
          Effect.persist(AddWorker(workerId, req.name)).thenReply(replyTo) { _ => WorkerRegistered(workerId) }
        case GetWorkers(replyTo) => Effect.none.thenReply(replyTo) { state => WorkersList(state.workers.values.toList) }
        case WorkerReportGpu(workerId, gpuInfo, replyTo) =>
          Effect.persist(UpdateWorkerGpu(workerId, gpuInfo))
            .thenReply(replyTo) { _ => () }
        case WorkerTaskDispatch(expInstance, replyTo) =>
          val assignments = assignGraphs(expInstance.taskGraphs.keys.toList, state.workers.keys.toList)
          val events: Iterable[Event] =
            for ((graphId, workerId) <- assignments)
              yield AppendWorkerTask(
                workerId,
                RunnableGraph.fromTaskGraph(expInstance, expInstance.taskGraphs(graphId))
              )
          Effect.persist(events.toSeq).thenReply(replyTo) { _ =>
            WorkerTaskDispatched(expInstance.expId, assignments)
          }
        case WorkerTaskFetch(workerId, replyTo) =>
          state.workers.get(workerId) match {
            case Some(info) => Effect.persist(CleanupWorkerTask(workerId)).thenReply(replyTo) { _ =>
              WorkerTaskFetched(info.pendingTasks)
            }
            case None => Effect.none
          }

        case WorkerReportResult(_, resultInfo, replyTo) =>
          Effect.none.thenRun { state =>
            replyTo ! ()
            state.expManager match {
              case Some(ref) => ref ! ExpManager.WorkerReportResult(resultInfo)
              case None => context.log.error("Result loss due to missing ExpManager ref.")
            }
          }
      },
      eventHandler = (state, evt) => evt match {
        case FindExpManager(actorRef) => (State.expManager set Some(actorRef)) { state }
        case AddWorker(workerId, name) =>
          val maybeTasks = state.workers filter { case (_, i) => i.name == name } map { case (_, i) => i.pendingTasks }
          val f =
            State.workers ^|-> at(workerId) set Some(WorkerInfo(workerId, name, Nil, maybeTasks.headOption getOrElse Nil))
          val g =
            State.workers modify { workers =>
              workers.filter { case (_, workerInfo) =>
                workerInfo.name != name
              }
            }
          f(g(state))
        case UpdateWorkerGpu(workerId, gpuInfo) =>
          val f =
            State.workers ^|-? index(workerId) ^|-> WorkerInfo.gpuStatus set gpuInfo
          f(state)
        case AppendWorkerTask(workerId, taskGraph) =>
          val f =
            State.workers ^|-? index(workerId) ^|-> WorkerInfo.pendingTasks modify { x => x.appended(taskGraph) }
          f(state)
        case CleanupWorkerTask(workerId) =>
          val f =
            State.workers ^|-? index(workerId) ^|-> WorkerInfo.pendingTasks set Nil
          f(state)
      }
    )
      .snapshotWhen { case (_, _, _) => false }
      .withRetention(RetentionCriteria.snapshotEvery(
        numberOfEvents = 50,
        keepNSnapshots = 2
      ))
  }

  def assignGraphs(graphs: List[String], workers: List[String]): Map[String, String] = {
    val rounds = (graphs.length.toDouble / workers.length.toDouble).ceil.toInt
    graphs.zip { Tools.duplicate(workers, rounds) }.toMap
  }
}
