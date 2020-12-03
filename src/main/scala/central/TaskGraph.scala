package central

import java.util.UUID

import monocle.macros.Lenses

@Lenses
case class TaskGraph(graphId: String, nodes: List[TaskInstance], dependencies: List[(String, String)])

// task graph
object TaskGraph {
  // build task graph from Json task
  def fromJsTask(task: JsTask): TaskGraph = {
    def go(task: JsTask): (List[String], List[String], List[TaskInstance], List[(String, String)]) = task match {
      case t @ JsSingletonTask(_, _, _) =>
        val inst = TaskInstance.fromJsTask(t)
        (List(inst.taskId), List(inst.taskId), List(inst), Nil)
      case JsParTask(subtasks) =>
        val ys = subtasks map go
        (ys flatMap { x => x._1 }, ys flatMap { x => x._2 }, ys flatMap { x => x._3 }, ys flatMap { x => x._4 })
      case JsSeqTask(subtasks) =>
        val ys = subtasks map go
        val dependencies =
          (0 until (ys.length - 1)).flatMap { i =>
            val left = ys(i)._2
            val right = ys(i + 1)._1
            for (x <- left; y <- right)
              yield (x -> y)
          }
        (ys.head._1, ys.last._2, ys flatMap { x => x._3 }, ys.flatMap { x => x._4 } ++ dependencies)
    }
    val res = go(task)
    TaskGraph(UUID.randomUUID.toString, res._3, res._4)
  }
}
