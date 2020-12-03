package sircle_lang

import OpType._

// desugar for binary operator
object BinOpDesugar extends TreeWalkDesugar {
  val opTable: Map[OpType, String] = Map(
    AND -> "__and",
    OR -> "__or",
    PLUS -> "__add",
    MINUS -> "__subtract",
    MUL -> "__mult",
    DIV -> "__div",
    IN -> "__in",
    LT -> "__lt",
    LE -> "__le",
    GT -> "__gt",
    GE -> "__ge",
    EQ -> "__eq",
    NEQ -> "__neq",
    GET -> "__get",
    SEQ -> "__seq",
    PAR -> "__par",
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
