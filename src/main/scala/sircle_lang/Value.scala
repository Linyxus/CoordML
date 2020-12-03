package sircle_lang

import spray.json._

// Sircle value base class
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

case class ValTask(task: Task) extends Value {
  override val valueType: ValueType = TaskType
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
    case ValMapping(pairs) => s"{ ${pairs map { x => s"${x._1} -> ${Value show x._2}" } mkString ", "} }"
    case ValBuiltin(argSig, _, _) => s"<builtin func with signature ${argSig.map(_.toString) mkString " -> "}>"
    case ValTask(task) => Task show task
  }

  def fromJson(jsValue: JsValue): Value = jsValue match {
    case JsObject(fields) => ValMapping(fields.map { x =>
      x._1 -> fromJson(x._2)
    })
    case JsArray(elements) => ValList(elements.toList map fromJson)
    case JsString(value) => ValString(value)
    case JsNumber(value) =>
      if (value.isValidInt) ValInt(value.intValue)
      else ValDouble(value.doubleValue)
    case boolean: JsBoolean => ValBoolean(boolean.value)
    case JsNull => throw new NotImplementedError(s"Converting from json null to Value is not supported.")
  }

  def toJson(value: Value): JsValue = value match {
    case ValInt(value) => JsNumber(value)
    case ValDouble(value) => JsNumber(value)
    case ValString(value) => JsString(value)
    case ValBoolean(value) => JsBoolean(value)
    case ValList(items) => JsArray((items map toJson).toVector)
    case ValTuple(items) => JsArray((items map toJson).toVector)
    case ValMapping(pairs) => JsObject(pairs.map { x =>
      x._1 -> toJson(x._2)
    })
    case ValTask(task) => Task toJson task
    case v => throw new NotImplementedError(s"Converting from Value ${Value show v} to json is not supported.")
  }
}

