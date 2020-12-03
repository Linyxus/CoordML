package sircle_lang

// for combinator in for expression
sealed trait ForCombinator

case class ForBind(name: String, expr: Expr) extends ForCombinator

case class ForFilter(expr: Expr) extends ForCombinator

object ForCombinator {
  def show(combinator: ForCombinator): String = combinator match {
    case ForBind(name, expr) => s"$name <- ${Expr show expr}"
    case ForFilter(expr) => Expr show expr
  }
}
