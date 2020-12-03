package sircle_lang

import scala.annotation.tailrec

sealed trait ValueType {
  //  def ===(that: ValueType): Boolean = {
  //    (this, that) match {
  //      case (AnyType, _) | (_, AnyType) => true
  //      case (ListType(x), ListType(y)) => x === y
  //      case (LambdaType(x), LambdaType(y)) => x === y
  //      case _ => this == that
  //    }
  //  }
  //

  // check whether that is a subtype of this
  def <==(that: ValueType): Boolean = {
    this == that
  }
}

// any type is a subtype of AnyType
case object AnyType extends ValueType {
  override def <==(that: ValueType): Boolean = true
}

case object IntType extends ValueType

case object DoubleType extends ValueType

case object StringType extends ValueType

case object UnitType extends ValueType

case object BooleanType extends ValueType

case class LambdaType(argType: ValueType) extends ValueType {
  override def <==(that: ValueType): Boolean = that match {
    case LambdaType(thatArgType) => argType <== thatArgType
    case _ => false
  }
}

case object ListType extends ValueType

case class TupleType(itemTypes: List[ValueType]) extends ValueType {
  override def <==(that: ValueType): Boolean = that match {
    case TupleType(thatItemTypes) =>
      itemTypes.length == thatItemTypes.length &&
        itemTypes.zip(thatItemTypes).forall { x => x._1 <== x._2 }
    case _ => false
  }
}

case class MappingType(structures: List[(String, ValueType)]) extends ValueType {
  override def <==(that: ValueType): Boolean = that match {
    case MappingType(thatPairs) =>
      @tailrec
      def go(xs: List[(String, ValueType)]): Boolean = xs match {
        case x :: rem => thatPairs find { p => p._1 == x._1 } match {
          case Some(t) => (t._2 <== x._2) && go(rem)
          case None => false
        }
        case Nil => true
      }

      go(this.structures)
    case _ => false
  }
}

case object TaskType extends ValueType

object ValueType {
  def show(valueType: ValueType): String = valueType match {
    case AnyType => "*"
    case IntType => "Int"
    case DoubleType => "Double"
    case StringType => "String"
    case UnitType => "()"
    case BooleanType => "Boolean"
    case LambdaType(argType) => s"${show(argType)} -> *"
    case ListType => s"[*]"
    case TupleType(itemTypes) => s"( ${itemTypes map show mkString ", "} )"
    case MappingType(structures) => s"{ ${structures map { x => s"${x._1} : ${show(x._2)}" } mkString ", "} }"
  }
}
