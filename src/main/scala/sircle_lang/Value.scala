package sircle_lang

sealed trait Value

case class ValInt(value: Int) extends Value

case class ValDouble(value: Double) extends Value

case class ValString(value: String) extends Value

case object ValUnit extends Value

case class ValLambda(name: String, expr: Expr) extends Value

case class ValIdentifier(name: String) extends Value

object Value {
  def showValue(value: Value): String = value match {
    case ValInt(v) => v.toString
    case ValDouble(v) => v.toString
    case ValString(v) => "\"" + v + "\""
    case ValUnit => "()"
    case ValLambda(name, expr) => s"lambda: $name => ${Expr.showExpr(expr)}"
    case ValIdentifier(name) => name
  }
}
