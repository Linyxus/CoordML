package sircle_lang

sealed trait Binding

case class ValBinding(name: String, valType: TypeExpr, expr: Expr) extends Binding

case class ExprBinding(expr: Expr) extends Binding
