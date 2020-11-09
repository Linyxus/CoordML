package sircle_lang

sealed trait Value {
  val valueType: ValueType
}

case class ValInt(value: Int) extends Value {
  override val valueType: ValueType = IntType
}

case class ValDouble(value: Double) extends Value {
  override val valueType: ValueType = DoubleType
}

case class ValString(value: String) extends Value {
  override val valueType: ValueType = StringType
}

case object ValUnit extends Value {
  override val valueType: ValueType = UnitType
}

case class ValBoolean(value: Boolean) extends Value {
  override val valueType: ValueType = BooleanType
}

case class ValLambda(name: String, expr: Expr) extends Value {
  override val valueType: ValueType = ???
}

object Value {
  def showValue(value: Value): String = value match {
    case ValInt(v) => v.toString
    case ValDouble(v) => v.toString
    case ValString(v) => "\"" + v + "\""
    case ValUnit => "()"
    case ValBoolean(v) => if (v) "True" else "False"
    case ValLambda(name, expr) => s"lambda: $name => ${Expr.showExpr(expr)}"
  }
}
