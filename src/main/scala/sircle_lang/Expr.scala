package sircle_lang

// expression type
sealed trait Expr

case class ExprBinary(left: Expr, op: OpType.OpType, right: Expr) extends Expr

case class ExprUnary(op: PrefixType.PrefixType, expr: Expr) extends Expr

case class ExprValue(value: Value) extends Expr

case class ExprIdentifier(name: String) extends Expr

case class ExprLambda(argName: String, argType: TypeExpr, body: Expr) extends Expr

case class ExprApp(func: Expr, arg: Expr) extends Expr

case class ExprList(items: List[Expr]) extends Expr

case class ExprTuple(items: List[Expr]) extends Expr

case class ExprMapping(pairs: List[(String, Expr)]) extends Expr

case class ExprBlock(effects: List[Effect]) extends Expr

case class ExprIf(cond: Expr, left: Expr, right: Option[Expr]) extends Expr

case class ExprFor(combinators: List[ForCombinator], expr: Expr) extends Expr

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
    case ExprBlock(lines) => s"{ ${lines map Effect.show mkString " ; "} }"
    case ExprIf(cond, left, right) => s"if ${show(cond)} then ${show(left)}" + (right match {
      case None => ""
      case Some(value) => s" else ${show(value)}"
    })
    case ExprFor(combinators, expr) => s"for ${combinators map ForCombinator.show mkString ", "} do ${show(expr)}"
    case ExprMapping(pairs) => s"<Mapping { ${pairs map { x => s"${x._1} -> ${Expr show x._2}" } mkString ", "} }>"
  }
}
