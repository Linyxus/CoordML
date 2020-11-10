package sircle_lang

class Evaluator {
  val prelude: Map[String, Value] = Map(
    "add" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValInt(x.asInstanceOf[ValInt].value + y.asInstanceOf[ValInt].value)
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

  def resolveIdentifier(name: String): Value =
    prelude get name match {
      case None => throw RuntimeError(s"Can not find identifier $name.")
      case Some(v) => v
    }

  def evalExpr(expr: Expr): Value =
    expr match {
      case ExprValue(value) => value
      case ExprIdentifier(name) => resolveIdentifier(name)
      case ExprApp(func, arg) => {
        val f = evalExpr(func)
        val x = evalExpr(arg)
        val xT = x.valueType
        val expectT = LambdaType(xT)
        if (expectT === f.valueType) {
          f match {
            case ValBuiltin(argSig, func, resolved) =>
              val resolvedP = resolved :+ x
              if (resolvedP.length == argSig.length) {
                func(resolvedP)
              } else {
                ValBuiltin(argSig, func, resolvedP)
              }
            case ValLambda(_, _) => throw RuntimeError("Unimplemented for applying lambda.")
            case _ => throw RuntimeError("Error in type checker.")
          }
        } else
          throw RuntimeError("Type mismatch.")
      }
      case _ => throw RuntimeError("Unimplemented")
    }
}
