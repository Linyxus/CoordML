package central

import java.util.UUID

case class TaskInstance(taskId: String,
                        executable: String,
                        args: Map[String, String],
                        meta: Map[String, String],
                        status: TaskStatus)

object TaskInstance {
  def fromJsTask(task: JsSingletonTask): TaskInstance =
    TaskInstance(
      taskId = UUID.randomUUID().toString,
      executable = task.executable,
      args = task.args,
      meta = task.meta,
      status = TaskStatusTodo
    )
}
