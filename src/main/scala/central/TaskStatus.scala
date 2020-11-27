package central

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

