package central

import scala.util.Random

import monocle.macros.Lenses

/**
 * Task instance.
 * @param taskId Unique identifier.
 * @param executable Executable to run the task.
 * @param args Args to be supplied to the executble.
 * @param meta Meta tag to identify task results.
 * @param status Task status.
 */
@Lenses
case class TaskInstance(taskId: String,
                        executable: String,
                        args: Map[String, String],
                        meta: Map[String, String],
                        status: TaskStatus)

object TaskInstance {
  // build task instance from Json task
  def fromJsTask(task: JsSingletonTask): TaskInstance =
    TaskInstance(
      taskId = Random.alphanumeric.take(4).mkString ++ HexDigest().of(task).take(8),
      executable = task.executable,
      args = task.args,
      meta = task.meta,
      status = TaskStatusTodo
    )
}
