package sircle_lang

import scala.collection.immutable.List

class Evaluator {
  val valuePrelude: List[(String, Value)] = List(
    "add" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValInt(x.asInstanceOf[ValInt].value + y.asInstanceOf[ValInt].value)
      }
    ),
    "add" -> ValBuiltin(
      List(StringType, StringType),
      args => {
        val x :: y :: Nil = args
        ValString(x.asInstanceOf[ValString].value + y.asInstanceOf[ValString].value)
      }
    ),
    "add" -> ValBuiltin(
      List(ListType(AnyType), ListType(AnyType)),
      args => {
        val x :: y :: Nil = args
        val lx = x.asInstanceOf[ValList]
        val ly = y.asInstanceOf[ValList]
        if (lx.itemType === ly.itemType) {
          ValList(lx.itemType, lx.items ++ ly.items)
        } else {
          throw RuntimeError(s"Can not concat two lists of different types: ${lx.itemType} and ${ly.itemType}.")
        }
      }
    ),
    "subtract" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValInt(x.asInstanceOf[ValInt].value - y.asInstanceOf[ValInt].value)
      }
    ),
    "mult" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValInt(x.asInstanceOf[ValInt].value * y.asInstanceOf[ValInt].value)
      }
    ),
    "div" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValInt(x.asInstanceOf[ValInt].value / y.asInstanceOf[ValInt].value)
      }
    ),
    "andF" -> ValBuiltin(
      List(BooleanType, BooleanType),
      args => {
        val x :: y :: Nil = args
        ValBoolean(x.asInstanceOf[ValBoolean].value && y.asInstanceOf[ValBoolean].value)
      }
    ),
    "orF" -> ValBuiltin(
      List(BooleanType, BooleanType),
      args => {
        val x :: y :: Nil = args
        ValBoolean(x.asInstanceOf[ValBoolean].value || y.asInstanceOf[ValBoolean].value)
      }
    ),
    "notF" -> ValBuiltin(
      List(BooleanType),
      args => {
        val x :: Nil = args
        ValBoolean(!x.asInstanceOf[ValBoolean].value)
      }
    ),
    "negF" -> ValBuiltin(
      List(IntType),
      args => {
        val x :: Nil = args
        ValInt(-x.asInstanceOf[ValInt].value)
      }
    ),
    "cons" -> ValBuiltin(
      List(AnyType, ListType(AnyType)),
      args => {
        val x :: xs :: Nil = args
        val t = x.valueType
        val xsT = xs.valueType.asInstanceOf[ListType]
        if (t === xsT.itemType) {
          ValList(t, x :: xs.asInstanceOf[ValList].items)
        } else {
          throw RuntimeError(s"Type mismatch in cons, $t and $xsT")
        }
      }
    ),
    "elemOf" -> ValBuiltin(
      List(AnyType, ListType(AnyType)),
      args => {
        val x :: xs :: Nil = args
        val t = x.valueType
        val l = xs.asInstanceOf[ValList]
        val xsT = l.itemType
        if (t === xsT) {
          ValBoolean(l.items contains x)
        } else {
          throw RuntimeError(s"Type mismatch in elemOf, $t and $xsT")
        }
      }
    ),
    "isEmpty" -> ValBuiltin(
      List(ListType(AnyType)),
      args => {
        val x :: Nil = args
        ValBoolean(x.asInstanceOf[ValList].items.isEmpty)
      }
    ),
    "head" -> ValBuiltin(
      List(ListType(AnyType)),
      args => {
        val x :: Nil = args
        val l = x.asInstanceOf[ValList].items
        if (l.isEmpty) {
          throw RuntimeError("Can not obtain the head of an empty list.")
        } else
          l.head
      }
    ),
    "tail" -> ValBuiltin(
      List(ListType(AnyType)),
      args => {
        val x :: Nil = args
        val l = x.asInstanceOf[ValList].items
        if (l.isEmpty) {
          throw RuntimeError("Can not obtain the tail of an empty list.")
        } else
          ValList(x.asInstanceOf[ValList].itemType, l.tail)
      }
    ),
  )

  val typePrelude: Map[String, ValueType] = Map(
    "Any" -> AnyType,
    "Int" -> IntType,
    "Double" -> DoubleType,
    "String" -> StringType,
    "Unit" -> UnitType,
    "Boolean" -> BooleanType,
  )

  def locateList[A, B](key: A, l: List[(A, B)], pred: B => Boolean = (x: Any) => true): B =
    l find { x =>
      x._1 == key && pred(x._2)
    } match {
      case Some(value) => value._2
      case None => throw RuntimeError(s"Can not locate name $key.")
    }

  def locateValue(name: String, local: List[(String, Value)], pred: Value => Boolean = _ => true): Value =
    locateList(name, local :++ valuePrelude, pred)

  def locateType(name: String, local: List[(String, ValueType)] = Nil): ValueType =
    locateList(name, local :++ typePrelude)

  def evalTypeExpr(typeExpr: TypeExpr): ValueType = typeExpr match {
    case TypeExprIdentifier(name) => locateType(name)
    case TypeExprList(itemType) => ListType(evalTypeExpr(itemType))
    case TypeExprArrow(left, _) => LambdaType(evalTypeExpr(left))
  }

  def evalExpr(expr: Expr, localVal: List[(String, Value)] = Nil): Value =
    expr match {
      case ExprValue(value) => value
      case ExprIdentifier(name) => locateValue(name, localVal)
      case ExprLambda(argName, argType, body) =>
        val t = evalTypeExpr(argType)
        ValLambda(argName, t, body, localVal)
      case ExprApp(func, arg) =>
        val x = evalExpr(arg, localVal)
        val xT = x.valueType
        val expectT = LambdaType(xT)
        val f = func match {
          case ExprIdentifier(name) => locateValue(name, localVal, expectT === _.valueType)
          case _ => evalExpr(func, localVal)
        }
        if (expectT === f.valueType) {
          f match {
            case ValBuiltin(argSig, func, resolved) =>
              val resolvedP = resolved :+ x
              if (resolvedP.length == argSig.length) {
                func(resolvedP)
              } else {
                ValBuiltin(argSig, func, resolvedP)
              }
            case ValLambda(argName, _, body, lexical) =>
              val innerLocal = (argName -> x) :: (lexical :++ localVal)
              evalExpr(body, innerLocal)
            case _ => throw RuntimeError("Error in type checker.")
          }
        } else
          throw RuntimeError("Type mismatch.")

      case _ => throw RuntimeError("Unimplemented")
    }
}
