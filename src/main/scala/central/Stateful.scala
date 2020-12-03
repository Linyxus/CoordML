package central
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.persistence.typed.PersistenceId
import akka.actor.typed.{ActorRef, Behavior}

// test stateful actor
object Stateful {
  sealed trait Command
  case class Add(replyTo: ActorRef[Int]) extends Command
  case class Show(replyTo: ActorRef[Int]) extends Command

  sealed trait Event
  case object Increment extends Event with JacksonEvt

  def apply(): Behavior[Command] =
    EventSourcedBehavior[Command, Event, Int](
      persistenceId = PersistenceId.ofUniqueId("stateful-inc"),
      emptyState = 0,
      commandHandler = (_, cmd) => cmd match {
        case Add(replyTo) =>
          Effect.persist(Increment).thenReply(replyTo) { identity }
        case Show(replyTo) => Effect.none.thenReply(replyTo) { identity }
      },
      eventHandler = (state, evt) => evt match {
        case Increment => state + 1
      }
    )
}
