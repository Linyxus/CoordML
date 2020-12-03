package central

import sircle_lang.{RuntimeError, ValTask, ParseError}

case class ResultView(rowKey: List[String], columnKey: List[String])

/**
 * Blueprint of an experiment
 * @param title
 * @param author
 * @param envPath
 * @param resultParser Parse rules used to extract results. Parse rules are defined in the Python parse package.
 * @param resultView Definition of result views.
 * @param task The task to be run.
 */
case class ExpBlueprint(title: String,
                        author: String,
                        envPath: String,
                        resultParser: String,
                        resultView: ResultView,
                        task: JsTask)

object ExpBlueprint {
  /**
   * Build ExpBlueprint from the request
   * @param request
   * @return The result blueprint
   */
  def fromRequest(request: CreateExpRequest): Either[String, ExpBlueprint] =
    try {
      println(s"Creating experiment from request $request")
      // compile the resolver
      val resolver = sircle_lang.Runtime.compileFunc(List(request.resolverPath), "resolve")
      println(s"Obtained resolver function $resolver")
      println(s"Parsing args from ${request.config}")
      // parse the config arguments
      val arg = sircle_lang.Value.fromJson(request.config)
      println(s"Parsed args $arg")
      // resolve the task
      val task = resolver(List(arg)) match {
        case ValTask(task) => task
        case v => throw RuntimeError(s"Returned value type of resolve is ${v.valueType}, expecting task.")
      }
      Right(ExpBlueprint(request.title, request.author, request.envPath, request.resultParse, request.resultView, JsTask fromTask task))
    } catch {
      case RuntimeError(msg) => Left("Runtime error: " + msg)
      case ParseError(msg) => Left("Parse error: " + msg)
      case e =>
        e.printStackTrace()
        Left("Unknown error.")
    }
}
