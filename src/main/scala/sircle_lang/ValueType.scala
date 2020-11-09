package sircle_lang

sealed trait ValueType {
  def checkType(that: ValueType): Boolean = that == this
}

case object IntType extends ValueType

case object DoubleType extends ValueType

case object StringType extends ValueType

case object UnitType extends ValueType

case object BooleanType extends ValueType

case class LambdaType(argType: ValueType) extends ValueType
