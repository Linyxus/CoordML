package central

import spray.json._
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import sircle_lang.{ParTask, SeqTask, SingletonTask}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[JsSeqTask], name = "jsSeqTask"),
  new JsonSubTypes.Type(value = classOf[JsParTask], name = "jsParTask"),
  new JsonSubTypes.Type(value = classOf[JsSingletonTask], name = "jsSingletonTask")
))
trait JsTask

case class JsSeqTask(subtasks: List[JsTask]) extends JsTask

case class JsParTask(subtasks: List[JsTask]) extends JsTask

case class JsSingletonTask(executable: String, args: Map[String, String], meta: Map[String, String]) extends JsTask

object JsTask {
  def fromTask(task: sircle_lang.Task): JsTask = task match {
    case SeqTask(subtasks) => JsSeqTask(subtasks map fromTask)
    case ParTask(subtasks) => JsParTask(subtasks map fromTask)
    case SingletonTask(executable, args, meta) => JsSingletonTask(
      executable,
      args.map { p => (p._1, sircle_lang.Value.show(p._2)) },
      meta.map { p => (p._1, sircle_lang.Value.show(p._2)) }
    )
  }

  def toJson(task: JsTask): JsValue = task match {
    case JsSeqTask(subtasks) => JsObject(
      "type" -> JsString("seq"),
      "subtasks" -> JsArray((subtasks map toJson).toVector)
    )
    case JsParTask(subtasks) => JsObject(
      "type" -> JsString("par"),
      "subtasks" -> JsArray((subtasks map toJson).toVector)
    )
    case JsSingletonTask(executable, args, meta) => JsObject(
      "type" -> JsString("singleton"),
      "executable" -> JsString(executable),
      "args" -> JsObject(args map { x => x._1 -> JsString(x._2) }),
      "meta" -> JsObject(meta map { x => x._1 -> JsString(x._2) }),
    )
  }
}
