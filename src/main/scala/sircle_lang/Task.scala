package sircle_lang

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
}
