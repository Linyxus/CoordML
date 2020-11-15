package sircle_lang

import scala.annotation.tailrec

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
    value.valueType == this
}

case object AnyType extends ValueType {
  override def <~~(value: Value): Boolean = true
}

case object IntType extends ValueType

case object DoubleType extends ValueType

case object StringType extends ValueType

case object UnitType extends ValueType

case object BooleanType extends ValueType

case class LambdaType(argType: ValueType) extends ValueType

case class ListType(itemType: ValueType) extends ValueType {
  override def <~~(value: Value): Boolean = if (itemType == AnyType) value.valueType match {
    case ListType(_) => true
    case _ => false
  } else super.<~~(value)
}

case class TupleType(itemTypes: List[ValueType]) extends ValueType

case class MappingType(structures: List[(String, ValueType)]) extends ValueType {
  override def <~~(value: Value): Boolean = value.valueType match {
    case MappingType(that) =>
      @tailrec
      def go(xs: List[(String, ValueType)]): Boolean = xs match {
        case x :: rem => that find { p => p._1 == x._1 } match {
          case Some(t) => t._2 == x._2 && go(rem)
          case None => false
        }
        case Nil => true
      }
      go(this.structures)
    case _ => false
  }
}
