package sircle_lang

sealed trait Binding

case class ValBinding(name: String, valType: TypeExpr, expr: Expr) extends Binding

case class ExprBinding(expr: Expr) extends Binding

object Binding {
  def show(binding: Binding): String = binding match {
    case ValBinding(name, valType, expr) => s"bind $name: ${TypeExpr show valType} to ${Expr show expr}"
    case ExprBinding(expr) => s"eval expr ${Expr show expr}"
  }
}
