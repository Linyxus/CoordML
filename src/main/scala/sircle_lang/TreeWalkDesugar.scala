package sircle_lang

trait TreeWalkDesugar {
  def desugar(expr: Expr): Expr
  def transform(expr: Expr): Expr = expr match {
    case ExprBinary(left, op, right) => desugar(ExprBinary(transform(left), op, transform(right)))
    case ExprUnary(op, expr) => desugar(ExprUnary(op, transform(expr)))
    case ExprApp(func, arg) => desugar(ExprApp(transform(func), transform(arg)))
    case ExprLambda(argName, argType, body) => desugar(ExprLambda(argName, argType, transform(body)))
    case ExprList(items) => desugar(ExprList(items map transform))
    case ExprTuple(items) => desugar(ExprTuple(items map transform))
    case ExprIf(cond, left, right) => desugar(ExprIf(transform(cond), transform(left), right map transform))
    case ExprMapping(pairs) => desugar(ExprMapping(pairs map { x => x._1 -> transform(x._2) }))
    case x => desugar(x)
  }
}
