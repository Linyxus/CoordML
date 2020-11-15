package sircle_lang

object OpType extends Enumeration {
  type OpType = Value
  val
    DOLLAR, MAPS_TO, SEQ, PAR,
    AND, OR, GT, LT, GE, LE, EQ, NEQ,
    PLUS, MINUS, MUL, DIV, IN, GET
    = Value

  type Associativity = Value
  val LeftAssoc, RightAssoc = Value
}
