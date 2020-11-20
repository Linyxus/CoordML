package central
import sircle_lang._
import java.util.UUID.randomUUID

sealed trait TaskStatus
case object TaskStatusTodo extends TaskStatus
case class TaskStatusDone(results: Map[String, Double]) extends TaskStatus

sealed trait TaskInstance
case class ParTaskInstance(subtasks: List[TaskInstance]) extends TaskInstance
case class SeqTaskInstance(subtasks: List[TaskInstance]) extends TaskInstance
class SingletonTaskInstance(val taskId: String, var status: TaskStatus, val task: sircle_lang.SingletonTask) extends TaskInstance

class ExpInstance(val expId: String,
                  val blueprint: ExpBlueprint,
                  val taskInstances: List[(String, TaskInstance)])

object TaskInstance {
  def fromTask(task: Task): TaskInstance = task match {
    case SeqTask(subtasks) => SeqTaskInstance(subtasks map fromTask)
    case ParTask(subtasks) => ParTaskInstance(subtasks map fromTask)
    case t @ SingletonTask(_, _, _) =>
      new SingletonTaskInstance(randomUUID().toString, TaskStatusTodo, t)
  }
}

object ExpInstance {
  def splitParTask(task: Task): List[Task] = task match {
    case ParTask(subtasks) => subtasks flatMap splitParTask
    case x => x :: Nil
  }

  def getTaskInstances(task: Task): List[TaskInstance] = splitParTask(task) map TaskInstance.fromTask

  def fromBluePrint(blueprint: ExpBlueprint): ExpInstance = new ExpInstance(
    expId = randomUUID().toString,
    blueprint = blueprint,
    taskInstances = Nil
  )
}
