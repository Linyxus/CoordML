package central

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import sircle_lang.{Evaluator, ParseError, RuntimeError}
import sircle_lang.Value.{show, toJson}
import spray.json._

object Functional {

  sealed trait Command

  final case class EvalSircleExpr(expr: String, replyTo: ActorRef[EvalSircleExprResponse]) extends Command

  final case class SessEvalSircleExpr(expr: String, sessId: Int, replyTo: ActorRef[EvalSircleExprResponse]) extends Command

  final case class JsonToSircleValue(jsValue: JsValue, replyTo: ActorRef[JsonToSircleValueResponse]) extends Command

  final case class CreateSession(replyTo: ActorRef[SessionCreated]) extends Command

  final case class EvalSircleExprResponse(maybeResult: Option[JsValue])

  final case class JsonToSircleValueResponse(maybeResult: Option[String])

  final case class SessionCreated(sessId: Int)

  def apply(): Behavior[Command] = call(Nil)

  private def call(sessions: List[sircle_lang.Runtime]): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      message match {
        case EvalSircleExpr(expr, replyTo) =>
          context.log.info(s"eval expr $expr.")
          try {
            val runtime = sircle_lang.Runtime(Nil)
            val res = toJson(runtime.loadSource(expr))
            replyTo ! EvalSircleExprResponse(Some(res))
          } catch {
            case _ => replyTo ! EvalSircleExprResponse(None)
          }
          Behaviors.same

        case SessEvalSircleExpr(expr, sessId, replyTo) =>
          try {
            val runtime = sessions(sessId)
            val res = toJson(runtime.loadSource(expr))
            replyTo ! EvalSircleExprResponse(Some(res))
          } catch {
            case _ => replyTo ! EvalSircleExprResponse(None)
          }
          Behaviors.same

        case JsonToSircleValue(jsValue, replyTo) =>
          try {
            val value = sircle_lang.Value.fromJson(jsValue)
            replyTo ! JsonToSircleValueResponse(Some(show(value)))
          } catch {
            case _ => replyTo ! JsonToSircleValueResponse(None)
          }
          Behaviors.same

        case CreateSession(replyTo) =>
          val runtime = sircle_lang.Runtime(Nil)
          val sessId = sessions.length
          replyTo ! SessionCreated(sessId)
          call(sessions :+ runtime)
      }
    }
}
