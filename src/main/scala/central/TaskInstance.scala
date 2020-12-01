package central

import scala.util.Random
import java.security.MessageDigest

import monocle.macros.Lenses

@Lenses
case class TaskInstance(taskId: String,
                        executable: String,
                        args: Map[String, String],
                        meta: Map[String, String],
                        status: TaskStatus)

object TaskInstance {
  def fromJsTask(task: JsSingletonTask): TaskInstance =
    TaskInstance(
      taskId = Random.alphanumeric.take(4).mkString ++ HexDigest().of(task).take(8),
      executable = task.executable,
      args = task.args,
      meta = task.meta,
      status = TaskStatusTodo
    )
}
