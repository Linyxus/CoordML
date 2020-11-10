package sircle_lang

trait TreeWalkDesugar {
  def desugar(expr: Expr): Expr
  def transform(expr: Expr): Expr = expr match {
    case ExprBinary(left, op, right) => desugar(ExprBinary(transform(left), op, transform(right)))
    case ExprUnary(op, expr) => desugar(ExprUnary(op, transform(expr)))
    case ExprApp(func, arg) => desugar(ExprApp(transform(func), transform(arg)))
    case ExprLambda(argName, argType, body) => desugar(ExprLambda(argName, argType, transform(body)))
    case ExprList(items) => desugar(ExprList(items map transform))
    case x => desugar(x)
  }
}
