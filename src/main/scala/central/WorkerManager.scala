package central

import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.persistence.typed.PersistenceId
import akka.actor.typed.{ActorRef, Behavior}
import java.util.UUID.randomUUID

final case class GpuMemUsage(used: Int, capacity: Int)

final case class GpuInfo(name: String, memUsage: GpuMemUsage, load: Double)

final case class WorkerInfo(workerId: String, gpuStatus: List[GpuInfo], pendingTasks: List[ExpInstance])

object WorkerManager {

  sealed trait Command

  final case class WorkerRegister(replyTo: ActorRef[WorkerRegistered]) extends Command

  final case class WorkerRegistered(workerId: String)

  final case class GetWorkers(replyTo: ActorRef[WorkersList]) extends Command

  final case class WorkersList(workers: List[WorkerInfo])

  final case class WorkerReportGpu(workerId: String, gpuInfo: List[GpuInfo], replyTo: ActorRef[Unit]) extends Command

  final case class WorkerTaskDispatch(expInstance: ExpInstance, tasks: List[TaskInstance],
                                      replyTo: ActorRef[WorkerTaskDispatched]) extends Command

  final case class WorkerTaskDispatched(expId: String, tasks: List[(String, TaskInstance)])

  final case class WorkerTaskFetch(workerId: String, replyTo: ActorRef[WorkerTaskFetched]) extends Command

  final case class WorkerTaskFetched(tasks: List[ExpInstance])

  sealed trait Event

  final case class AddWorker(workerId: String) extends Event with JacksonEvt

  final case class UpdateWorkerGpu(workerId: String, gpuInfo: List[GpuInfo]) extends Event with JacksonEvt

  final case class AppendWorkerTask(workerId: String, expInstance: ExpInstance) extends Event with JacksonEvt

  final case class CleanupWorkerTask(workerId: String) extends Event with JacksonEvt

  final case class State(workers: Map[String, WorkerInfo])

  def apply(): Behavior[Command] =
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
        case WorkerTaskDispatch(expInstance, tasks, replyTo) =>
          val workerIds = state.workers.keys.toList
          val numWorkers = workerIds.length
          val numPar = tasks.length
          val perWorker = (numPar.toDouble / numWorkers.toDouble).ceil.toInt
          val taskSeg = tasks.grouped(perWorker)
          val assignments = workerIds zip taskSeg
          Effect.persist(assignments map { x =>
            AppendWorkerTask(x._1, ExpInstance(expInstance.expId, expInstance.blueprint, x._2 map { t => (x._1, t) }))
          }).thenReply(replyTo) { _ =>
            WorkerTaskDispatched(
              expInstance.expId,
              assignments.flatMap { x =>
                x._2 map { t => (x._1, t) }
              }
            )
          }
        case WorkerTaskFetch(workerId, replyTo) =>
          state.workers.get(workerId) match {
            case Some(info) => Effect.persist(CleanupWorkerTask(workerId)).thenReply(replyTo) { _ =>
              WorkerTaskFetched(info.pendingTasks)
            }
            case None => Effect.none
          }
      },
      eventHandler = (state, evt) => evt match {
        case AddWorker(workerId) => State(state.workers.updated(workerId, WorkerInfo(workerId, List.empty, List.empty)))
        case UpdateWorkerGpu(workerId, gpuInfo) => State(state.workers.updatedWith(workerId) { mx =>
          mx.map(info => WorkerInfo(info.workerId, gpuInfo, info.pendingTasks))
        })
        case AppendWorkerTask(workerId, expInstance) => State(state.workers.updatedWith(workerId) { m =>
          m.map { i =>
            WorkerInfo(i.workerId, i.gpuStatus, i.pendingTasks.appended(expInstance))
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
