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

case class ValLambda(argName: String, argType: ValueType, expr: Expr, local: List[(String, Value)]) extends Value {
  override val valueType: ValueType = LambdaType(argType)
}

case class ValBuiltin(argSig: List[ValueType], func: List[Value] => Value, resolved: List[Value] = Nil) extends Value {
  override val valueType: ValueType = LambdaType(argSig(resolved.length))
}

case class ValList(items: List[Value]) extends Value {
  override val valueType: ValueType = ListType
}

case class ValTuple(items: List[Value]) extends Value {
  override val valueType: ValueType = TupleType(items.map(_.valueType))
}

case class ValMapping(pairs: Map[String, Value]) extends Value {
  override val valueType: ValueType = MappingType(pairs.toList map { x => (x._1, x._2.valueType) })
}

object Value {
  def show(value: Value): String = value match {
    case ValInt(v) => v.toString
    case ValDouble(v) => v.toString
    case ValString(v) => "\"" + v + "\""
    case ValUnit => "()"
    case ValBoolean(v) => if (v) "True" else "False"
    case ValLambda(argName, argType, expr, _) => s"lambda: $argName: $argType => ${Expr.show(expr)}"
    case ValList(items) => s"[${items map show mkString ", "}]"
    case ValTuple(items) => s"(${items map show mkString ", "})"
    case ValMapping(pairs) => s"{ ${pairs map { x => s"${x._1} -> ${Value show x._2}" } mkString ", " } }"
    case ValBuiltin(argSig, func, _) => s"<builtin func with signature ${ argSig.map(_.toString) mkString " -> " }>"
  }
}
