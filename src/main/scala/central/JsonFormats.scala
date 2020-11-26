package central

import central.Functional.SessionCreated
import sircle_lang.Task
import sircle_lang.Task.toJson
import spray.json._

object JsonFormats {

  import DefaultJsonProtocol._

  implicit object TaskFormat extends RootJsonFormat[sircle_lang.Task] {
    override def read(json: JsValue): Task =
      throw new NotImplementedError(s"Converting from Json to Task is not supported")

    override def write(obj: Task): JsValue = Task.toJson(obj)
  }

  implicit val sysInfoJsonFormat: RootJsonFormat[SysInfo] = jsonFormat1(SysInfo)

  implicit val sessionCreatedJsonFormat: RootJsonFormat[SessionCreated] = jsonFormat1(SessionCreated)

  implicit val workerRegisteredFormat: RootJsonFormat[WorkerManager.WorkerRegistered] =
    jsonFormat1(WorkerManager.WorkerRegistered)

  implicit val gpuMemUsageFormat: RootJsonFormat[GpuMemUsage] = jsonFormat2(GpuMemUsage)
  implicit val gpuInfoFormat: RootJsonFormat[GpuInfo] = jsonFormat3(GpuInfo)

  implicit val resultViewFormat: RootJsonFormat[ResultView] = jsonFormat2(ResultView)
  implicit val createExpRequestFormat: RootJsonFormat[CreateExpRequest] = jsonFormat7(CreateExpRequest)
  implicit val expCreatedFormat: RootJsonFormat[ExpManager.ExpCreated] = jsonFormat1(ExpManager.ExpCreated)

  implicit object TaskInstanceFormat extends RootJsonFormat[TaskInstance] {
    def write(taskInstance: TaskInstance): JsValue = taskInstance match {
      case ParTaskInstance(subtasks) => JsObject(
        "type" -> JsString("par"),
        "subtasks" -> JsArray((subtasks map write).toVector)
      )
      case SeqTaskInstance(subtasks) => JsObject(
        "type" -> JsString("seq"),
        "subtasks" -> JsArray((subtasks map write).toVector)
      )
      case instance: SingletonTaskInstance => JsObject(
        "type" -> JsString("singleton"),
        "task" -> JsTask.toJson(instance.jsTask),
        "status" -> (instance.status match {
          case TaskStatusDone(results) => results.toJson
          case TaskStatusTodo => JsNull
        })
      )
    }

    override def read(json: JsValue): TaskInstance =
      throw new NotImplementedError("Converting from Json to TaskInstance is not supported.")
  }

  implicit object ExpBlueprintFormat extends RootJsonFormat[ExpBlueprint] {
    override def read(json: JsValue): ExpBlueprint = ???

    override def write(obj: ExpBlueprint): JsValue = JsObject(
      "title" -> obj.title.toJson,
      "author" -> obj.author.toJson,
      "env_path" -> obj.envPath.toJson,
      "result_parse" -> obj.resultParser.toJson,
      "result_view" -> obj.resultView.toJson,
      "task" -> JsTask.toJson(obj.task)
    )
  }

  implicit object ExpInstanceFormat extends RootJsonFormat[ExpInstance] {
    override def read(json: JsValue): ExpInstance = ???

    override def write(obj: ExpInstance): JsValue = JsObject(
      "exp_id" -> obj.expId.toJson,
      "blueprint" -> obj.blueprint.toJson,
      "tasks" -> obj.taskInstances.toJson
    )
  }

  implicit val workerInfoFormat: RootJsonFormat[WorkerInfo] = jsonFormat3(WorkerInfo)
  implicit val workersListFormat: RootJsonFormat[WorkerManager.WorkersList] =
    jsonFormat1(WorkerManager.WorkersList)

  implicit val workerTaskFetched: RootJsonFormat[WorkerManager.WorkerTaskFetched] = jsonFormat1(WorkerManager.WorkerTaskFetched)
}
