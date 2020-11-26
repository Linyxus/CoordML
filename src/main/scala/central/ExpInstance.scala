package central

import java.lang.annotation.Annotation

import sircle_lang._
import java.util.UUID.randomUUID

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo, JsonTypeName}
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import com.fasterxml.jackson.databind.deser.std.StdDeserializer


@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[TaskStatusDone], name = "taskStatusDone"),
  new JsonSubTypes.Type(value = classOf[TaskStatusTodo], name = "taskStatusTodo")
))
sealed trait TaskStatus

@JsonDeserialize(using = classOf[TaskStatusTodoDeserializer])
sealed trait TaskStatusTodo extends TaskStatus
@JsonTypeName("taskStatusTodo")
case object TaskStatusTodo extends TaskStatusTodo

class TaskStatusTodoDeserializer extends StdDeserializer[TaskStatusTodo](TaskStatusTodo.getClass) {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext): TaskStatusTodo = TaskStatusTodo
}

case class TaskStatusDone(results: Map[String, Double]) extends TaskStatus

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[ParTaskInstance], name = "parTaskInstance"),
  new JsonSubTypes.Type(value = classOf[SeqTaskInstance], name = "seqTaskInstance"),
  new JsonSubTypes.Type(value = classOf[SingletonTaskInstance], name = "singletonTaskInstance")
))
sealed trait TaskInstance

case class ParTaskInstance(subtasks: List[TaskInstance]) extends TaskInstance

case class SeqTaskInstance(subtasks: List[TaskInstance]) extends TaskInstance

class SingletonTaskInstance(val taskId: String, var status: TaskStatus, val jsTask: JsSingletonTask) extends TaskInstance

case class ExpInstance(expId: String,
                       blueprint: ExpBlueprint,
                       taskInstances: List[(String, TaskInstance)])

object TaskInstance {
  def fromTask(task: JsTask): TaskInstance = task match {
    case JsSeqTask(subtasks) => SeqTaskInstance(subtasks map fromTask)
    case JsParTask(subtasks) => ParTaskInstance(subtasks map fromTask)
    case t@JsSingletonTask(_, _, _) =>
      new SingletonTaskInstance(randomUUID().toString, TaskStatusTodo, t)
  }
}

object ExpInstance {
  def splitParTask(task: JsTask): List[JsTask] = task match {
    case JsParTask(subtasks) => subtasks flatMap splitParTask
    case x => x :: Nil
  }

  def getTaskInstances(task: JsTask): List[TaskInstance] = splitParTask(task) map TaskInstance.fromTask

  def fromBluePrint(blueprint: ExpBlueprint): ExpInstance = new ExpInstance(
    expId = randomUUID().toString,
    blueprint = blueprint,
    taskInstances = Nil
  )
}
