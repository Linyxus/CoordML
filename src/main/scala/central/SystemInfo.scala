package central

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

final case class SysInfo(coordMLVersion: String)

object SystemInfo {
  sealed trait Command
  final case class GetInfo(replyTo: ActorRef[GetInfoResponse]) extends Command
  final case class GetInfoResponse(sysInfo: SysInfo)

  def apply(): Behavior[Command] = process

  private def process: Behavior[Command] =
    Behaviors.receiveMessage {
      case GetInfo(replyTo) =>
        val sysInfo = SysInfo("v0.0.1")
        replyTo ! GetInfoResponse(sysInfo)
        Behaviors.same
    }
}
