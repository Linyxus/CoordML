package sircle_lang

// Sircle language runtime
class Runtime {
  val evaluator = new Evaluator

  def loadSource(source: String): Value = {
    val bindings = Parser.parseSource(source)
    var ret: Value = ValUnit
    for (b <- bindings)
      ret = evaluator.executeBinding(b)
    ret
  }

  def loadFile(sourcePath: String): Unit = {
    val source = io.Source.fromFile(sourcePath).getLines mkString "\n"
    loadSource(source)
  }

  def evalExpr(expr: Expr): Value = evaluator.evalExpr(evaluator.desugarExpr(expr))

  loadSource(Preload.preloadSource)
}

object Runtime {
  def apply(sourcePaths: List[String]): Runtime = {
    val runtime = new Runtime
    for (path <- sourcePaths)
      runtime.loadFile(path)
    runtime
  }

  def compileFunc(files: List[String], funcName: String): List[Value] => Value = {
    def func(args: List[Value]): Value = {
      val expr = CodeCompose.applyFunc(ExprIdentifier(funcName), args map ExprValue)
      Runtime(files).evalExpr(expr)
    }
    func
  }
}
