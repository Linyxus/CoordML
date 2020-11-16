package sircle_lang

sealed trait TypeExpr

case class TypeExprIdentifier(name: String) extends TypeExpr

case class TypeExprList(itemType: TypeExpr) extends TypeExpr

case class TypeExprArrow(left: TypeExpr, right: TypeExpr) extends TypeExpr

case class TypeExprMapping(pairs: List[(String, TypeExpr)]) extends TypeExpr

object TypeExpr {
  def show(typeExpr: TypeExpr): String = typeExpr match {
    case TypeExprIdentifier(name) => s"@$name"
    case TypeExprList(itemType) => s"[${TypeExpr show itemType}]"
    case TypeExprArrow(left, right) => s"(${show(left)} -> ${show(right)})"
    case TypeExprMapping(pairs) => s"{ ${pairs map { x => s"${x._1} : ${TypeExpr show x._2}" } mkString ", " } }"
  }
}
