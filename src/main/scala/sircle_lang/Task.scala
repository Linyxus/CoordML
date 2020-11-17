package sircle_lang

import spray.json._

sealed trait Task

case class SeqTask(subtasks: List[Task]) extends Task

case class ParTask(subtasks: List[Task]) extends Task

case class SingletonTask(executable: String, args: Map[String, Value], meta: Map[String, Value]) extends Task

object Task {
  def show(task: Task): String = task match {
    case SeqTask(subtasks) => s"<SeqTask ${subtasks map show mkString " >> "} >"
    case ParTask(subtasks) => s"<ParTask ${subtasks map show mkString " || "} >"
    case SingletonTask(executable, args, meta) => s"<Task $executable args=$args meta=$meta>"
  }

  def toJson(task: Task): JsValue = task match {
    case SeqTask(subtasks) => JsObject(
      "type" -> JsString("seq"),
      "subtasks" -> JsArray((subtasks map toJson).toVector)
    )
    case ParTask(subtasks) => JsObject(
      "type" -> JsString("par"),
      "subtasks" -> JsArray((subtasks map toJson).toVector)
    )
    case SingletonTask(executable, args, meta) => JsObject(
      "type" -> JsString("singleton"),
      "executable" -> JsString(executable),
      "args" -> JsObject(args map { x => x._1 -> Value.toJson(x._2) }),
      "meta" -> JsObject(meta map { x => x._1 -> Value.toJson(x._2) }),
    )
  }
}
