package sircle_lang

sealed trait Expr

case class ExprBinary(left: Expr, op: OpType.OpType, right: Expr) extends Expr

case class ExprUnary(op: PrefixType.PrefixType, expr: Expr) extends Expr

case class ExprValue(value: Value) extends Expr

case class ExprApp(func: Expr, arg: Expr) extends Expr

object Expr {
  def showExpr(expr: Expr): String = expr match {
    case ExprBinary(left, op, right) if op == OpType.DOLLAR => s"(${showExpr(left)} ${showExpr(right)})"
    case ExprBinary(left, op, right) => s"($op ${showExpr(left)} ${showExpr(right)})"
    case ExprUnary(op, expr) => s"($op ${showExpr(expr)}"
    case ExprValue(value) => s"<${Value.showValue(value)}>"
    case ExprApp(func, arg) => s"(${showExpr(func)} ${showExpr(arg)})"
  }
}
