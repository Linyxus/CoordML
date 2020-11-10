package sircle_lang

import scala.annotation.tailrec

object CodeCompose {
  @tailrec
  def applyFunc(f: Expr, args: List[Expr]): Expr = args match {
    case Nil => f
    case x :: xs => applyFunc(ExprApp(f, x), xs)
  }
}
