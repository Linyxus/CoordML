package central

import java.util.UUID.randomUUID

import monocle.macros.Lenses

@Lenses
case class ExpInstance(expId: String,
                       blueprint: ExpBlueprint,
                       taskGraphs: Map[String, TaskGraph],
                       workerSchedule: Map[String, String])

object ExpInstance {
  def splitParTask(task: JsTask): List[JsTask] = task match {
    case JsParTask(subtasks) => subtasks flatMap splitParTask
    case x => x :: Nil
  }

  def getTaskGraphs(task: JsTask): List[TaskGraph] = splitParTask(task) map TaskGraph.fromJsTask

  def fromBluePrint(blueprint: ExpBlueprint): ExpInstance = new ExpInstance(
    expId = randomUUID().toString,
    blueprint = blueprint,
    taskGraphs = getTaskGraphs(blueprint.task).map { g => g.graphId -> g }.toMap,
    workerSchedule = Map.empty
  )
}
