package central

import sircle_lang.{RuntimeError, ValTask, ParseError}

case class ResultView(rowKey: List[String], columnKey: List[String])

case class ExpBlueprint(title: String,
                        author: String,
                        envPath: String,
                        resultParser: String,
                        resultView: ResultView,
                        task: JsTask)

object ExpBlueprint {
  def fromRequest(request: CreateExpRequest): Either[String, ExpBlueprint] =
    try {
      val resolver = sircle_lang.Runtime.compileFunc(List(request.resolverPath), "resolve")
      val arg = sircle_lang.Value.fromJson(request.config)
      val task = resolver(List(arg)) match {
        case ValTask(task) => task
        case v => throw RuntimeError(s"Returned value type of resolve is ${v.valueType}, expecting task.")
      }
      Right(ExpBlueprint(request.title, request.author, request.envPath, request.resultParse, request.resultView, JsTask fromTask task))
    } catch {
      case RuntimeError(msg) => Left("Runtime error: " + msg)
      case ParseError(msg) => Left("Parse error: " + msg)
      case _ => Left("Unknown error.")
    }
}
