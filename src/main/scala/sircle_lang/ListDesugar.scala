package sircle_lang

// list desugar for converting list expression to cons function application
object ListDesugar extends TreeWalkDesugar {
  override def desugar(expr: Expr): Expr = expr match {
    case ExprList(items) => transformList(items)
    case x => x
  }

  def transformList(expr: List[Expr]): Expr =
    expr match {
      case Nil => ExprValue(ValList(Nil))
      case x :: xs => ExprApp(ExprApp(ExprIdentifier("cons"), x), transformList(xs))
    }
}
