package central

import central.ExpManager.RenderedTaskListing
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

  implicit val resultInfoFormat: RootJsonFormat[ResultInfo] = jsonFormat4(ResultInfo)

  implicit object TaskInstanceFormat extends RootJsonFormat[TaskInstance] {
    def write(instance: TaskInstance): JsValue =
      JsObject(
        "taskId" -> instance.taskId.toJson,
        "args" -> instance.args.toJson,
        "meta" -> instance.meta.toJson,
        "executable" -> instance.executable.toJson,
        "status" -> (instance.status match {
          case TaskStatusDone(results) => results.toJson
          case TaskStatusTodo => JsNull
        })
      )

    override def read(json: JsValue): TaskInstance =
      throw new NotImplementedError("Converting from Json to TaskInstance is not supported.")
  }

  implicit object ExpBlueprintFormat extends RootJsonFormat[ExpBlueprint] {
    override def read(json: JsValue): ExpBlueprint = ???

    override def write(obj: ExpBlueprint): JsValue = JsObject(
      "title" -> obj.title.toJson,
      "author" -> obj.author.toJson,
      "envPath" -> obj.envPath.toJson,
      "resultParse" -> obj.resultParser.toJson,
      "resultView" -> obj.resultView.toJson,
      "task" -> JsTask.toJson(obj.task)
    )
  }

  implicit object TaskGraphFormat extends RootJsonFormat[TaskGraph] {
    override def write(obj: TaskGraph): JsValue = JsObject(
      "graphId" -> obj.graphId.toJson,
      "nodes" -> obj.nodes.toJson,
      "dependencies" -> obj.dependencies.toJson
    )

    override def read(json: JsValue): TaskGraph = ???
  }

  implicit object TaskRunnableFormat extends RootJsonFormat[TaskRunnable] {
    override def write(obj: TaskRunnable): JsValue = JsObject(
      "expId" -> obj.expId.toJson,
      "envPath" -> obj.envPath.toJson,
      "resultParse" -> obj.resultParse.toJson,
      "task" -> obj.taskInstance.toJson
    )

    override def read(json: JsValue): TaskRunnable = ???
  }

  implicit object RunnableGraphFormat extends RootJsonFormat[RunnableGraph] {
    override def write(obj: RunnableGraph): JsValue = JsObject(
      "graphId" -> obj.graphId.toJson,
      "nodes" -> obj.nodes.toJson,
      "dependencies" -> obj.dependencies.toJson
    )

    override def read(json: JsValue): RunnableGraph = ???
  }

  implicit object ExpInstanceFormat extends RootJsonFormat[ExpInstance] {
    override def read(json: JsValue): ExpInstance = ???

    override def write(obj: ExpInstance): JsValue = JsObject(
      "expId" -> obj.expId.toJson,
      "blueprint" -> obj.blueprint.toJson,
      "taskGraphs" -> obj.taskGraphs.toJson
    )
  }

  implicit object WorkerInfoFormat extends RootJsonFormat[WorkerInfo] {
    override def write(obj: WorkerInfo): JsValue = JsObject(
      "workerId" -> obj.workerId.toJson,
      "name" -> obj.name.toJson,
      "gpuStatus" -> obj.gpuStatus.toJson,
      "pendingTasks" -> obj.pendingTasks.toJson
    )

    override def read(json: JsValue): WorkerInfo = {
      val obj = json.asJsObject
      WorkerInfo(
        workerId = obj.fields("workerId").convertTo[String],
        gpuStatus = obj.fields("gpuStatus").convertTo[List[GpuInfo]],
        name = "",
        pendingTasks = Nil
      )
    }
  }
  implicit val workersListFormat: RootJsonFormat[WorkerManager.WorkersList] =
    jsonFormat1(WorkerManager.WorkersList)

  implicit val workerTaskFetched: RootJsonFormat[WorkerManager.WorkerTaskFetched] = jsonFormat1(WorkerManager.WorkerTaskFetched)

  implicit val statusResponseFormat: RootJsonFormat[WorkerManager.StatusResponse] = jsonFormat2(WorkerManager.StatusResponse)

  implicit val expOverviewResponseFormat: RootJsonFormat[ExpManager.ExpOverviewResponse] = jsonFormat4(ExpManager.ExpOverviewResponse)

  implicit val workerRegisterRequestFormat: RootJsonFormat[WorkerRegisterRequest] = jsonFormat1(WorkerRegisterRequest)

  implicit val expOverviewListingFormat: RootJsonFormat[ExpManager.ExpOverviewListing] = jsonFormat1(ExpManager.ExpOverviewListing)

  implicit val resultTableFormat: RootJsonFormat[ResultTable] = jsonFormat2(ResultTable)

  implicit val renderedTaskFormat: RootJsonFormat[RenderedTask] = jsonFormat5(RenderedTask)

  implicit val renderedTaskListingFormat: RootJsonFormat[RenderedTaskListing] = jsonFormat1(RenderedTaskListing)
}
