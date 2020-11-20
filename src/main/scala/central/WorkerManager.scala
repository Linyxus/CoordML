package central

import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.persistence.typed.PersistenceId
import akka.actor.typed.{ActorRef, Behavior}
import java.util.UUID.randomUUID

final case class GpuMemUsage(used: Int, capacity: Int)
final case class GpuInfo(name: String, memUsage: GpuMemUsage, load: Double)
final case class WorkerInfo(workerId: String, gpuStatus: List[GpuInfo])

object WorkerManager {
  sealed trait Command
  final case class WorkerRegister(replyTo: ActorRef[WorkerRegistered]) extends Command
  final case class WorkerRegistered(workerId: String)
  final case class GetWorkers(replyTo: ActorRef[WorkersList]) extends Command
  final case class WorkersList(workers: List[WorkerInfo])
  final case class WorkerReportGpu(workerId: String, gpuInfo: List[GpuInfo], replyTo: ActorRef[Unit]) extends Command

  sealed trait Event
  final case class AddWorker(workerId: String) extends Event
  final case class UpdateWorkerGpu(workerId: String, gpuInfo: List[GpuInfo]) extends Event

  final case class State(workers: Map[String, WorkerInfo])

  def apply(): Behavior[Command] =
    EventSourcedBehavior[Command, Event, State](
      persistenceId = PersistenceId.ofUniqueId("worker-manager"),
      emptyState = State(Map.empty),
      commandHandler = (_, cmd) => cmd match {
        case WorkerRegister(replyTo) =>
          val workerId = randomUUID().toString
          Effect.persist(AddWorker(workerId)).thenReply(replyTo) { _ => WorkerRegistered(workerId) }
        case GetWorkers(replyTo) => Effect.none.thenReply(replyTo) { state => WorkersList(state.workers.values.toList) }
        case WorkerReportGpu(workerId, gpuInfo, replyTo) =>
          Effect.persist(UpdateWorkerGpu(workerId, gpuInfo))
            .thenReply(replyTo) { _ => () }
      },
      eventHandler = (state, evt) => evt match {
        case AddWorker(workerId) => State(state.workers.updated(workerId, WorkerInfo(workerId, List.empty)))
        case UpdateWorkerGpu(workerId, gpuInfo) => State(state.workers.updatedWith(workerId) { mx =>
          mx.map(info => WorkerInfo(info.workerId, gpuInfo))
        })
      }
    )
}
