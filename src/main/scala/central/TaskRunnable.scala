package central

case class TaskRunnable(expId: String, envPath: String, resultParse: String, taskInstance: TaskInstance)

object TaskRunnable {
  def fromInstance(expInstance: ExpInstance, taskInstance: TaskInstance): TaskRunnable =
    TaskRunnable(
      expId = expInstance.expId,
      envPath = expInstance.blueprint.envPath,
      resultParse = expInstance.blueprint.resultParser,
      taskInstance = taskInstance
    )
}
