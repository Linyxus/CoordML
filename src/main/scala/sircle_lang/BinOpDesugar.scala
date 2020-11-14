package sircle_lang

import OpType._

object BinOpDesugar extends TreeWalkDesugar {
  val opTable: Map[OpType, String] = Map(
    AND -> "andF",
    OR -> "orF",
    PLUS -> "add",
    MINUS -> "subtract",
    MUL -> "mult",
    DIV -> "div",
    IN -> "elemOf",
    LT -> "lt",
    LE -> "le",
    GT -> "gt",
    GE -> "ge",
    EQ -> "eq",
  )

  override def desugar(expr: Expr): Expr = expr match {
    case ExprBinary(left, op, right) => op match {
      case DOLLAR => ExprApp(left, right)
      case op => opTable get op match {
        case Some(ident) => CodeCompose.applyFunc(ExprIdentifier(ident), left :: right :: Nil)
        case None => throw RuntimeError(s"Unsupported operator $op when desugaring binary expressions.")
      }
    }
    case x => x
  }
}
