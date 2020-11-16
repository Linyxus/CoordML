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
      List(ListType, ListType),
      args => {
        val x :: y :: Nil = args
        val lx = x.asInstanceOf[ValList]
        val ly = y.asInstanceOf[ValList]
        ValList(lx.items ++ ly.items)
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
    "eq" -> ValBuiltin(
      List(AnyType, AnyType),
      args => {
        val x :: y :: Nil = args
        (x, y) match {
          case (ValList(Nil), ValList(Nil)) => ValBoolean(true)
          case _ =>
            ValBoolean(x == y)
        }
      }
    ),
    "lt" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValBoolean(x.asInstanceOf[ValInt].value < y.asInstanceOf[ValInt].value)
      }
    ),
    "le" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValBoolean(x.asInstanceOf[ValInt].value <= y.asInstanceOf[ValInt].value)
      }
    ),
    "gt" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValBoolean(x.asInstanceOf[ValInt].value > y.asInstanceOf[ValInt].value)
      }
    ),
    "ge" -> ValBuiltin(
      List(IntType, IntType),
      args => {
        val x :: y :: Nil = args
        ValBoolean(x.asInstanceOf[ValInt].value >= y.asInstanceOf[ValInt].value)
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
      List(AnyType, ListType),
      args => {
        val x :: xs :: Nil = args
        ValList(x :: xs.asInstanceOf[ValList].items)
      }
    ),
    "elemOf" -> ValBuiltin(
      List(AnyType, ListType),
      args => {
        val x :: xs :: Nil = args
        val l = xs.asInstanceOf[ValList]
        ValBoolean(l.items contains x)
      }
    ),
    "isEmpty" -> ValBuiltin(
      List(ListType),
      args => {
        val x :: Nil = args
        ValBoolean(x.asInstanceOf[ValList].items.isEmpty)
      }
    ),
    "head" -> ValBuiltin(
      List(ListType),
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
      List(ListType),
      args => {
        val x :: Nil = args
        val l = x.asInstanceOf[ValList].items
        if (l.isEmpty) {
          throw RuntimeError("Can not obtain the tail of an empty list.")
        } else
          ValList(l.tail)
      }
    ),
    "length" -> ValBuiltin(
      List(ListType),
      args => {
        val x :: Nil = args
        val l = x.asInstanceOf[ValList].items
        ValInt(x.asInstanceOf[ValList].items.length)
      }
    ),
    "show" -> ValBuiltin(
      List(IntType),
      args => {
        val x :: Nil = args
        val l = x.asInstanceOf[ValInt].value
        ValString(l.toString)
      }
    ),
    "get" -> ValBuiltin(
      List(MappingType(Nil), StringType),
      args => {
        val fm :: fi :: Nil = args
        val m = fm.asInstanceOf[ValMapping]
        val i = fi.asInstanceOf[ValString]
        m.pairs get i.value match {
          case Some(value) => value
          case None => throw RuntimeError(s"Can not get field ${i.value} from mapping ${Value show fm}.")
        }
      }
    ),
    "keysOf" -> ValBuiltin(
      List(MappingType(Nil)),
      args => {
        val fm :: Nil = args
        val m = fm.asInstanceOf[ValMapping].pairs
        ValList(m.keys map ValString toList)
      }
    ),
    "buildMapping" -> ValBuiltin(
      List(ListType),
      args => {
        val fxs :: Nil = args
        val items = fxs.asInstanceOf[ValList].items.map { x =>
          if (TupleType(List(StringType, AnyType)) <== x.valueType) {
            val tuple = x.asInstanceOf[ValTuple]
            val ms :: mv :: Nil = tuple.items
            val s = ms.asInstanceOf[ValString].value
            (s, mv)
          } else throw RuntimeError(s"Unexpect item type ${x.valueType} when evaluating buildMapping.")
        }
        ValMapping(Map.from(items))
      }
    ),
    "getGlobalBindings" -> ValBuiltin(
      List(UnitType),
      _ => {
        ValMapping(Map.from(valueBindings))
      }
    ),
    "range" -> ValBuiltin(
      List(IntType),
      args => {
        val fn :: Nil = args
        val n = fn.asInstanceOf[ValInt].value
        ValList((0 until n).toList map ValInt)
      }
    ),
    "mult" -> ValBuiltin(
      List(ListType, ListType),
      args => {
        val fxs :: fys :: Nil = args
        val xs = fxs.asInstanceOf[ValList]
        val ys = fys.asInstanceOf[ValList]

        def f(x: Value, ys: List[Value]): List[Value] = ys map { y =>
          (x, y) match {
            case (ValTuple(items1), ValTuple(items2)) => ValTuple(items1 :++ items2)
            case (ValTuple(items1), v2) => ValTuple(items1 :+ v2)
            case (v1, ValTuple(items2)) => ValTuple(v1 :: items2)
            case (v1, v2) => ValTuple(v1 :: v2 :: Nil)
          }
        }

        ValList(xs.items flatMap { x => f(x, ys.items) })
      }
    )
  )

  type BindingEnv[A] = List[(String, A)]

  val typePrelude: Map[String, ValueType] = Map(
    "Any" -> AnyType,
    "Int" -> IntType,
    "Double" -> DoubleType,
    "String" -> StringType,
    "Unit" -> UnitType,
    "Boolean" -> BooleanType,
    "List" -> ListType,
  )

  var valueBindings: BindingEnv[Value] = List(
    "sysInfo" -> ValMapping(Map(
      "os" -> ValString("darwin"),
      "sircleVersion" -> ValString("v0.0.0"),
      "workingDir" -> ValString(System.getProperty("user.dir"))
    ))
  )

  var typeBindings: BindingEnv[ValueType] = Nil

  var variableEnv: SymbolBinding.Env = Nil

  def locateValueWithType(name: String, local: List[(String, Value)], expectType: ValueType): Value =
    (local :++ variableEnv.map { x => (x.name, x.value) } :++ valueBindings :++ valuePrelude).find { x =>
      x._1 == name && (x._2.valueType <== expectType)
    } match {
      case Some(value) => value._2
      case None => throw RuntimeError(s"Can not resolve symbol $name with type $expectType.")
    }

  def locateValue(name: String, localVal: List[(String, Value)]): Value =
    (localVal :++ variableEnv.map { x => (x.name, x.value) } :++ valueBindings :++ valuePrelude).find { x =>
      x._1 == name
    } match {
      case Some(value) => value._2
      case None => throw RuntimeError(s"Can not resolve symbol $name.")
    }

  def locateType(name: String, local: List[(String, ValueType)] = Nil): ValueType = {
    (local :++ typeBindings :++ typePrelude).find { x => x._1 == name } match {
      case Some(value) => value._2
      case None => throw RuntimeError(s"Can not resolve type alias $name.")
    }
  }

  def evalTypeExpr(typeExpr: TypeExpr): ValueType = typeExpr match {
    case TypeExprIdentifier(name) => locateType(name)
    case TypeExprArrow(left, _) => LambdaType(evalTypeExpr(left))
    case TypeExprMapping(pairs) => MappingType(
      Map.from(pairs).toList map { x =>
        (x._1, evalTypeExpr(x._2))
      })
    case TypeExprTuple(items) => TupleType(items map evalTypeExpr)
    case _ => throw RuntimeError(s"Unimplemented type expression ${TypeExpr show typeExpr}.")
  }

  def desugarExpr(expr: Expr): Expr = {
    var x = expr
    //    print("parsed: ")
    //    println(Expr show x)
    x = ListDesugar transform x
    //    print("list desugar: ")
    //    println(Expr show x)
    x = BinOpDesugar transform x
    //    print("binary op desugar: ")
    //    println(Expr show x)
    //    print("unary op desugar: ")
    x = UnaryOpDesugar transform x
    //    println(Expr show x)
    x
  }

  def executeBinding(binding: Binding, localVal: List[(String, Value)] = Nil): Value = binding match {
    case ValBinding(name, valType, expr) =>
      val t = evalTypeExpr(valType)

      (valuePrelude :++ Nil) find { x =>
        x._1 == name && (x._2.valueType <== t)
      } match {
        case Some(_) => throw RuntimeError(s"Can not override built-in symbol name $name.")
        case None =>
      }

      val v = getValue(expr, t, localVal)
      valueBindings = (name -> v) :: valueBindings
      v

    case ExprBinding(expr) =>
      val x = desugarExpr(expr)
      evalExpr(x, localVal)

    case TypeBinding(name, typeExpr) =>
      typePrelude find { x => x._1 == name } match {
        case Some(_) => throw RuntimeError(s"Can not override built-in type name $name.")
        case None =>
          val t = evalTypeExpr(typeExpr)
          typeBindings = (name -> t) :: typeBindings
          ValUnit
      }

    case _ => throw RuntimeError(s"Can not execute unimplemented binding: ${Binding show binding}.")
  }

  def getValue(expr: Expr, expect: ValueType, localVal: List[(String, Value)]): Value = {
    val v = evalExpr(desugarExpr(expr), localVal)
    if (expect <== v.valueType) v else throw RuntimeError(s"Type mismatch: $expect and ${v.valueType}.")
  }

  def executeEffect(effect: Effect, localVal: List[(String, Value)]): Value = effect match {
    case ValueBindEffect(name, valType, expr) =>
      val t = evalTypeExpr(valType)

      valuePrelude find { x => x._1 == name && (x._2.valueType <== t) } match {
        case Some(_) => throw RuntimeError(s"Cannot override built-in symbol name $name.")
        case None =>
          val v = getValue(expr, t, localVal)
          variableEnv = new SymbolBinding(name, v) :: variableEnv
          v
      }

    case AssignEffect(name, expr) =>
      val v = evalExpr(desugarExpr(expr), localVal)

      variableEnv.find(_.name == name) match {
        case Some(binding) =>
          if (binding.value.valueType <== v.valueType) {
            binding.value = v
            v
          } else throw RuntimeError(s"Type mismatch: assigning ${v.valueType} to ${binding.value.valueType} symbol.")
        case None => throw RuntimeError(s"Can not assign value to undefined symbol $name.")
      }

    case ExprEffect(expr) => evalExpr(desugarExpr(expr), localVal)
  }

  def mergeBindingEnv[A](left: BindingEnv[A], right: BindingEnv[A]): BindingEnv[A] =
    Map.from((left :++ right).reverse).toList

  def evalExpr(expr: Expr, localVal: BindingEnv[Value]): Value =
    expr match {
      case ExprValue(value) => value
      case ExprIdentifier(name) => locateValue(name, localVal)
      case ExprTuple(items) => ValTuple(items map (x => evalExpr(x, localVal)))
      case ExprBlock(bindings) => bindings match {
        case Nil => ValUnit
        case _ =>
          val origEnv = variableEnv
          val ret = (bindings map (x => executeEffect(x, localVal))).last
          variableEnv = origEnv
          ret
      }
      case ExprMapping(pairs) => ValMapping(Map from {
        pairs map { x =>
          x._1 -> evalExpr(x._2, localVal)
        }
      })

      case ExprLambda(argName, argType, body) =>
        val t = evalTypeExpr(argType)
        val variableBinding = variableEnv.map { x => (x.name, x.value) }
        ValLambda(argName, t, body, mergeBindingEnv(localVal, variableBinding))

      case ExprApp(func, arg) =>
        val x = evalExpr(arg, localVal)

        (func match {
          case ExprIdentifier(name) => locateValueWithType(name, localVal, LambdaType(x.valueType))
          case _ => evalExpr(func, localVal)
        }) match {
          case ValBuiltin(argSig, func, resolved) =>
            val resolvedP = resolved :+ x
            if (resolvedP.length == argSig.length) {
              func(resolvedP)
            } else {
              ValBuiltin(argSig, func, resolvedP)
            }
          case ValLambda(argName, _, body, lexical) =>
            val innerLocal = (argName -> x) :: mergeBindingEnv(lexical, localVal)
            evalExpr(body, innerLocal)
          case _ => throw RuntimeError("Error in type checker.")
        }

      case ExprIf(cond, left, right) =>
        val c = evalExpr(cond, localVal)
        if (c.valueType == BooleanType) {
          if (c.asInstanceOf[ValBoolean].value) {
            evalExpr(left, localVal)
          } else right match {
            case Some(value) => evalExpr(value, localVal)
            case None => ValUnit
          }
        } else throw RuntimeError(s"Expect if condition type Boolean, but got ${c.valueType}.")

      case ExprFor(combinators, expr) =>
        val forEnv = generateForEnvs(combinators, localVal)
        forEnv.map { x => evalExpr(desugarExpr(expr), x :++ localVal) } match {
          case Nil => ValList(Nil)
          case xs =>
            ValList(xs)
        }

      case _ => throw RuntimeError(s"Unimplemented expr ${Expr show expr}.")
    }

  def generateForEnvs(comb: List[ForCombinator], localVal: BindingEnv[Value]): List[BindingEnv[Value]] = {
    var ret: List[BindingEnv[Value]] = List(Nil)
    for (c <- comb) {
      c match {
        case ForBind(name, expr) =>
          ret = extendForEnvs(ret, name, expr, localVal)
        case ForFilter(expr) =>
          ret = filterForEnvs(ret, expr, localVal)
      }
    }
    ret
  }

  def extendForEnvs(env: List[BindingEnv[Value]], name: String, expr: Expr, localVal: BindingEnv[Value]): List[BindingEnv[Value]] =
    env flatMap { x => extendForEnv(x, name, expr, localVal) }

  def extendForEnv(env: BindingEnv[Value], name: String, expr: Expr, localVal: BindingEnv[Value]): List[BindingEnv[Value]] =
    env find { x => x._1 == name } match {
      case Some(_) => throw RuntimeError(s"Duplicated symbol $name in for expansion.")
      case None =>
        val v = evalExpr(desugarExpr(expr), env :++ localVal)
        if (ListType <== v.valueType)
          v.asInstanceOf[ValList].items.map { x => (name -> x) :: env }
        else
          throw RuntimeError(s"Expecting lists but found ${v.valueType} in for expansion.")
    }

  def filterForEnvs(env: List[BindingEnv[Value]], expr: Expr, localVal: BindingEnv[Value]): List[BindingEnv[Value]] =
    env.filter { x =>
      val v = evalExpr(desugarExpr(expr), x :++ localVal)
      if (v.valueType == BooleanType)
        v.asInstanceOf[ValBoolean].value
      else
        throw RuntimeError(s"Unexpected value type ${v.valueType} in for filter.")
    }
}
