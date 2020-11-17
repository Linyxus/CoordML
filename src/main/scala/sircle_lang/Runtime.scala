package sircle_lang

class Runtime {
  val evaluator = new Evaluator

  def loadSource(source: String): Unit = {
    val effects = Parser.parseSource(source)
    for (eff <- effects)
      evaluator.executeEffect(eff, Nil)
  }

  def loadFile(sourcePath: String): Unit = {
    val source = io.Source.fromFile(sourcePath) mkString "\n"
    loadSource(source)
  }

  def evalExpr(expr: Expr): Value = evaluator.evalExpr(evaluator.desugarExpr(expr), Nil)
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
