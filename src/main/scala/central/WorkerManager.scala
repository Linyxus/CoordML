package central

import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.persistence.typed.PersistenceId
import akka.actor.typed.{ActorRef, Behavior}
import java.util.UUID.randomUUID

import akka.actor.typed.scaladsl.Behaviors

final case class GpuMemUsage(used: Int, capacity: Int)

final case class GpuInfo(name: String, memUsage: GpuMemUsage, load: Double)

final case class WorkerInfo(workerId: String, gpuStatus: List[GpuInfo], pendingTasks: List[RunnableGraph])

final case class ResultInfo(expId: String, taskId: String, results: Map[String, String])

object WorkerManager {

  sealed trait Command

  final case class WorkerRegister(replyTo: ActorRef[WorkerRegistered]) extends Command

  final case class WorkerRegistered(workerId: String)

  final case class GetWorkers(replyTo: ActorRef[WorkersList]) extends Command

  final case class WorkersList(workers: List[WorkerInfo])

  final case class WorkerReportGpu(workerId: String, gpuInfo: List[GpuInfo], replyTo: ActorRef[Unit]) extends Command

  final case class WorkerTaskDispatch(expInstance: ExpInstance,
                                      replyTo: ActorRef[WorkerTaskDispatched]) extends Command

  final case class WorkerTaskDispatched(expId: String, tasks: Map[String, String])

  final case class WorkerTaskFetch(workerId: String, replyTo: ActorRef[WorkerTaskFetched]) extends Command

  final case class WorkerTaskFetched(tasks: List[RunnableGraph])

  final case class WorkerReportResult(workerId: String, resultInfo: ResultInfo, replyTo: ActorRef[Unit]) extends Command

  sealed trait Event

  final case class AddWorker(workerId: String) extends Event with JacksonEvt

  final case class UpdateWorkerGpu(workerId: String, gpuInfo: List[GpuInfo]) extends Event with JacksonEvt

  final case class AppendWorkerTask(workerId: String, runnableGraph: RunnableGraph) extends Event with JacksonEvt

  final case class CleanupWorkerTask(workerId: String) extends Event with JacksonEvt

  final case class State(workers: Map[String, WorkerInfo])

  def apply(): Behavior[Command] = Behaviors.setup[Command] { context =>
    EventSourcedBehavior[Command, Event, State](
      persistenceId = PersistenceId.ofUniqueId("worker-manager"),
      emptyState = State(Map.empty),
      commandHandler = (state, cmd) => cmd match {
        case WorkerRegister(replyTo) =>
          val workerId = randomUUID().toString
          Effect.persist(AddWorker(workerId)).thenReply(replyTo) { _ => WorkerRegistered(workerId) }
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

        case WorkerReportResult(workerId, resultInfo, replyTo) =>
          // report the result
          Effect.none.thenReply(replyTo) { _ => () }
      },
      eventHandler = (state, evt) => evt match {
        case AddWorker(workerId) => State(state.workers.updated(workerId, WorkerInfo(workerId, List.empty, List.empty)))
        case UpdateWorkerGpu(workerId, gpuInfo) => State(state.workers.updatedWith(workerId) { mx =>
          mx.map(info => WorkerInfo(info.workerId, gpuInfo, info.pendingTasks))
        })
        case AppendWorkerTask(workerId, taskGraph) => State(state.workers.updatedWith(workerId) { m =>
          m.map { i =>
            WorkerInfo(i.workerId, i.gpuStatus, i.pendingTasks.appended(taskGraph))
          }
        })
        case CleanupWorkerTask(workerId) => State(
          workers = state.workers.updatedWith(workerId) {
            case None => None
            case Some(info) => Some(WorkerInfo(info.workerId, info.gpuStatus, Nil))
          }
        )
      }
    )
  }

  def assignGraphs(graphs: List[String], workers: List[String]): Map[String, String] = {
    val rounds = (graphs.length.toDouble / workers.length.toDouble).ceil.toInt
    graphs.zip { Tools.duplicate(workers, rounds) }.toMap
  }
}
