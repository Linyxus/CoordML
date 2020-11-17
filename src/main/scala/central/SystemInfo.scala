package central

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import spray.json._

final case class SysInfo(coordMLVersion: String)

object SystemInfo {
  sealed trait Command
  final case class GetSysInfo(replyTo: ActorRef[GetSysInfoResponse]) extends Command
  final case class GetSysInfoResponse(sysInfo: SysInfo)

  def apply(): Behavior[Command] = process

  private def process: Behavior[Command] =
    Behaviors.receiveMessage {
      case GetSysInfo(replyTo) =>
        val sysInfo = SysInfo("v0.0.1")
        replyTo ! GetSysInfoResponse(sysInfo)
        Behaviors.same
    }
}
