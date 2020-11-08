package sircle_lang

import OpType.{Associativity, LeftAssoc, OpType, RightAssoc}
import PrefixType.PrefixType
import TokenType.TokenType

class Parser(val tokens: List[Token]) {
  val opRules: Array[(Associativity, Map[TokenType, OpType])] = Array(
    (RightAssoc, Map(
      TokenType.DOLLAR -> OpType.DOLLAR
    )),
    //    (RightAssoc, Map(
    //      TokenType.EQ_GT -> OpType.MAPS_TO
    //    )),
    (LeftAssoc, Map(
      TokenType.AND -> OpType.AND,
      TokenType.OR -> OpType.OR
    )),
    (LeftAssoc, Map(
      TokenType.GT -> OpType.GT,
      TokenType.LT -> OpType.LT,
      TokenType.GT_EQ -> OpType.GE,
      TokenType.LT_EQ -> OpType.LE,
      TokenType.EQ_EQ -> OpType.EQ,
      TokenType.BANG_EQ -> OpType.NEQ
    )),
    (LeftAssoc, Map(
      TokenType.PLUS -> OpType.PLUS,
      TokenType.MINUS -> OpType.MINUS
    )),
    (LeftAssoc, Map(
      TokenType.ASTERISK -> OpType.MUL,
      TokenType.SLASH -> OpType.DIV
    )),
    (LeftAssoc, Map(
      TokenType.IN -> OpType.IN
    ))
  )

  var current: List[Token] = tokens

  def eof: Boolean = current match {
    case Nil => true
    case x :: _ if x.tokenType == TokenType.EOF => true
    case _ => false
  }

  def peek: Token = current.head

  def advance: Token = {
    val token = peek
    if (!eof) current = current.tail
    token
  }

  def matchAhead(tokenType: TokenType): Boolean =
    if (peek.tokenType == tokenType) {
      advance
      true
    } else {
      false
    }

  def lookAhead(tokenTypes: List[TokenType]): Boolean =
    tokenTypes contains peek.tokenType


  val unaryRules: Map[TokenType, PrefixType] = Map(
    TokenType.NOT -> PrefixType.NOT,
    TokenType.MINUS -> PrefixType.NEG
  )

  def parseExpr: Expr = parseBinary(0)

  def parseBinary(level: Int): Expr = {
    if (level == opRules.length) {
      parseUnary
    } else {
      val (assoc, rules) = opRules(level)

      assoc match {
        case LeftAssoc =>
          var expr = parseBinary(level + 1)
          var found = true
          while (found) {
            rules get peek.tokenType match {
              case None => found = false
              case Some(op) =>
                advance
                expr = ExprBinary(expr, op, parseBinary(level + 1))
            }
          }
          expr
        case RightAssoc =>
          val expr = parseBinary(level + 1)
          rules get peek.tokenType match {
            case None => expr
            case Some(op) =>
              advance
              ExprBinary(expr, op, parseBinary(level))
          }
      }
    }
  }

  def parseUnary: Expr =
    unaryRules get peek.tokenType match {
      case None => parseApp
      case Some(op) =>
        advance
        ExprUnary(op, parseApp)
    }

  def parseApp: Expr = {
    val first = List(
      TokenType.LEFT_PAREN,
      TokenType.STRING,
      TokenType.INT,
      TokenType.DOUBLE,
      TokenType.UNIT,
      TokenType.IDENTIFIER
    )

    var expr = parseTerm
    var found = true

    while (found)
      if (lookAhead(first)) {
        expr = ExprApp(expr, parseTerm)
      } else {
        found = false
      }

    expr
  }

  def parseTerm: Expr = {
    val token = advance
    token.tokenType match {
      case TokenType.LEFT_PAREN =>
        val expr = parseExpr
        if (matchAhead(TokenType.RIGHT_PAREN)) {
          expr
        } else {
          throw ParseError("Unmatched parens")
        }
      case TokenType.INT => ExprValue(ValInt(token.lexeme.asInstanceOf[Int]))
      case TokenType.DOUBLE => ExprValue(ValDouble(token.lexeme.asInstanceOf[Double]))
      case TokenType.STRING => ExprValue(ValString(token.lexeme.asInstanceOf[String]))
      case TokenType.UNIT => ExprValue(ValUnit)
      case TokenType.IDENTIFIER =>
        ExprValue(if (matchAhead(TokenType.EQ_GT)) {
          ValLambda(token.content, parseExpr)
        } else {
          ValIdentifier(token.content)
        })

      case _ =>
        throw ParseError(s"Unexpected token: $token")
    }
  }
}
