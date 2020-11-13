package sircle_lang

sealed trait Expr

case class ExprBinary(left: Expr, op: OpType.OpType, right: Expr) extends Expr

case class ExprUnary(op: PrefixType.PrefixType, expr: Expr) extends Expr

case class ExprValue(value: Value) extends Expr

case class ExprIdentifier(name: String) extends Expr

case class ExprLambda(argName: String, argType: TypeExpr, body: Expr) extends Expr

case class ExprApp(func: Expr, arg: Expr) extends Expr

case class ExprList(items: List[Expr]) extends Expr

case class ExprTuple(items: List[Expr]) extends Expr

case class ExprBlock(bindings: List[Binding]) extends Expr

object Expr {
  def show(expr: Expr): String = expr match {
    case ExprBinary(left, op, right) if op == OpType.DOLLAR => s"(${show(left)} ${show(right)})"
    case ExprBinary(left, op, right) => s"($op ${show(left)} ${show(right)})"
    case ExprUnary(op, expr) => s"($op ${show(expr)})"
    case ExprValue(value) => s"<${Value.show(value)}>"
    case ExprIdentifier(name) => s"@$name"
    case ExprLambda(argName, argType, body) => s"<Lambda $argName: ${TypeExpr show argType} => ${Expr show body}>"
    case ExprApp(func, arg) => s"(${show(func)} ${show(arg)})"
    case ExprList(items) => s"<List [${items map show mkString ","}]>"
    case ExprTuple(items) => s"<Tuple (${items map show mkString ","})>"
    case ExprBlock(lines) => s"{ ${lines map Binding.show mkString " ; "} }"
  }
}
