package sircle_lang

sealed trait ValueType {
  def ===(that: ValueType): Boolean = {
    (this, that) match {
      case (AnyType, _) | (_, AnyType) => true
      case (ListType(x), ListType(y)) => x === y
      case (LambdaType(x), LambdaType(y)) => x === y
      case _ => this == that
    }
  }

  def <~~(value: Value): Boolean =
    value.valueType === this
}

case object AnyType extends ValueType

case object IntType extends ValueType

case object DoubleType extends ValueType

case object StringType extends ValueType

case object UnitType extends ValueType

case object BooleanType extends ValueType

case class LambdaType(argType: ValueType) extends ValueType

case class ListType(itemType: ValueType) extends ValueType

case class TupleType(itemTypes: List[ValueType]) extends ValueType
