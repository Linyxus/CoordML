package sircle_lang

// binding for variable symbol
sealed trait Binding

case class ValBinding(name: String, valType: TypeExpr, expr: Expr) extends Binding

case class ExprBinding(expr: Expr) extends Binding

case class TypeBinding(name: String, typeExpr: TypeExpr) extends Binding

object Binding {
  // convert binding to display string
  def show(binding: Binding): String = binding match {
    case ValBinding(name, valType, expr) => s"bind $name: ${TypeExpr show valType} := ${Expr show expr}"
    case ExprBinding(expr) => s"eval expr ${Expr show expr}"
    case TypeBinding(name, typeExpr) => s"type alias $name := ${TypeExpr show typeExpr}"
  }
}
