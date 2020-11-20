package central

import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.persistence.typed.PersistenceId
import akka.actor.typed.{ActorRef, Behavior}

import spray.json.JsValue

case class CreateExpRequest(title: String, author: String, config: JsValue, resolverPath: String,
                            envPath: String, resultParse: String, resultView: ResultView)

object ExpManager {

  sealed trait Command
  final case class ExpCreate(request: CreateExpRequest, replyTo: ActorRef[Either[String, ExpCreated]]) extends Command
  final case class ExpCreated(expId: String)

  sealed trait Event
  final case class AddExpInstance(expInstance: ExpInstance) extends Event

  final case class State(expInstances: Map[String, ExpInstance])

  def apply(): Behavior[Command] =
    EventSourcedBehavior[Command, Event, State](
      persistenceId = PersistenceId.ofUniqueId("exp-manager"),
      emptyState = State(Map.empty),
      commandHandler = (_, cmd) => cmd match {
        case ExpCreate(request, replyTo) =>
          ExpBlueprint.fromRequest(request) map ExpInstance.fromBluePrint match {
            case Left(value) => Effect.none.thenReply(replyTo) { _ => Left(value) }
            case Right(value) => Effect.persist(AddExpInstance(value)).thenRun { _ =>
              replyTo ! Right(ExpCreated(value.expId))
              // TODO notify the worker actor to dispatch tasks
            }
          }
      },
      eventHandler = (state, evt) => evt match {
        case AddExpInstance(expInstance) => State(state.expInstances.updated(expInstance.expId, expInstance))
      }
    )
}
