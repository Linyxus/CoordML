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

case class ValLambda(argName: String, argType: ValueType, expr: Expr) extends Value {
  override val valueType: ValueType = LambdaType(argType)
}

case class ValBuiltin(argSig: List[ValueType], func: List[Value] => Value, resolved: List[Value] = Nil) extends Value {
  override val valueType: ValueType = LambdaType(argSig(resolved.length))
}

case class ValList(itemType: ValueType, items: List[Value]) extends Value {
  override val valueType: ValueType = ListType(itemType)
}

object Value {
  def showValue(value: Value): String = value match {
    case ValInt(v) => v.toString
    case ValDouble(v) => v.toString
    case ValString(v) => "\"" + v + "\""
    case ValUnit => "()"
    case ValBoolean(v) => if (v) "True" else "False"
    case ValLambda(name, expr) => s"lambda: $name => ${Expr.show(expr)}"
    case ValList(itemType, items) => s"[${items map showValue mkString ", "}]:$itemType"
  }
}
