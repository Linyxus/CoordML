package sircle_lang

import PrefixType._

object UnaryOpDesugar extends TreeWalkDesugar {
  val opTable: Map[PrefixType, String] = Map(
    NOT -> "notF",
    NEG -> "negF"
  )

  override def desugar(expr: Expr): Expr = expr match {
    case ExprUnary(op, expr) => opTable get op match {
      case Some(ident) => CodeCompose.applyFunc(ExprIdentifier(ident), expr :: Nil)
      case None => throw RuntimeError(s"Unsupported prefix type $op when desugaring unary expressions.")
    }
    case x => x
  }
}
