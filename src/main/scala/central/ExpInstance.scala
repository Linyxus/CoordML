package central

import java.util.UUID.randomUUID

import monocle.macros.Lenses

/**
 * Experiment instance.
 * @param expId The unique id of the experiment.
 * @param blueprint The blueprint of the experiment.
 * @param taskGraphs Task graphs that can be run in parallel.
 * @param workerSchedule Task graph assignments.
 */
@Lenses
case class ExpInstance(expId: String,
                       blueprint: ExpBlueprint,
                       taskGraphs: Map[String, TaskGraph],
                       workerSchedule: Map[String, String])

object ExpInstance {
  /**
   * Split top-level parallel tasks.
   * @param task The task to be splitted.
   * @return The splitted task list.
   */
  def splitParTask(task: JsTask): List[JsTask] = task match {
    case JsParTask(subtasks) => subtasks flatMap splitParTask
    case x => x :: Nil
  }

  // convert task into task graphs
  def getTaskGraphs(task: JsTask): List[TaskGraph] = splitParTask(task) map TaskGraph.fromJsTask

  def fromBluePrint(blueprint: ExpBlueprint): ExpInstance = new ExpInstance(
    expId = randomUUID().toString, // get random experiment ID
    blueprint = blueprint,
    taskGraphs = getTaskGraphs(blueprint.task).map { g => g.graphId -> g }.toMap,
    workerSchedule = Map.empty
  )

  // extract tasks from the instance
  def tasksOf(expInstance: ExpInstance): List[TaskInstance] =
    expInstance.taskGraphs.values.flatMap { x => x.nodes } .toList
}
