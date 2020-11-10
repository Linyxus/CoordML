package sircle_lang

object ListDesugar extends TreeWalkDesugar {
  override def desugar(expr: Expr): Expr = expr match {
    case ExprList(items) => transformList(items)
    case x => x
  }

  def transformList(expr: List[Expr]): Expr =
    expr match {
      case Nil => ExprValue(ValList(AnyType, Nil))
      case x :: xs => ExprApp(ExprApp(ExprIdentifier("cons"), x), transformList(xs))
    }
}
