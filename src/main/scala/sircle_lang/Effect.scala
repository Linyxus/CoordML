package sircle_lang

sealed trait Effect

case class ValueBindEffect(name: String, valType: TypeExpr, expr: Expr) extends Effect

case class AssignEffect(name: String, expr: Expr) extends Effect

case class ExprEffect(expr: Expr) extends Effect

object Effect {
  def show(effect: Effect): String = effect match {
    case ValueBindEffect(name, valType, expr) => s"<Effect $name: ${TypeExpr show valType} := ${Expr show expr}>"
    case AssignEffect(name, expr) => s"<Effect $name <- ${Expr show expr}>"
    case ExprEffect(expr) => s"<Effect eval ${Expr show expr}>"
  }
}
