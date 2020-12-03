package central

/**
 * Runnable task.
 * @param expId Unique identifier.
 * @param envPath Environment path.
 * @param resultParse Parse rules for parsing the results.
 * @param taskInstance The associated task instance.
 */
case class TaskRunnable(expId: String, envPath: String, resultParse: String, taskInstance: TaskInstance)

object TaskRunnable {
  // build runnable task from exp and task instances
  def fromInstance(expInstance: ExpInstance, taskInstance: TaskInstance): TaskRunnable =
    TaskRunnable(
      expId = expInstance.expId,
      envPath = expInstance.blueprint.envPath,
      resultParse = expInstance.blueprint.resultParser,
      taskInstance = taskInstance
    )
}
