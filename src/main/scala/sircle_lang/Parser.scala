package sircle_lang

import OpType.{Associativity, LeftAssoc, OpType, RightAssoc}
import PrefixType.PrefixType
import TokenType.{IDENTIFIER, KW_DEF, TokenType}

// grammar parsers
class Parser(val tokens: List[Token]) {
  // binary operator parsing rules
  val opRules: Array[(Associativity, Map[TokenType, OpType])] = Array(
    (RightAssoc, Map(
      TokenType.DOLLAR -> OpType.DOLLAR
    )),
    (RightAssoc, Map(
      TokenType.GT_GT -> OpType.SEQ,
      TokenType.BAR_BAR -> OpType.PAR
    )),
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
    )),
    (LeftAssoc, Map(
      TokenType.DOT -> OpType.GET
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

  def expect[A](tokenType: TokenType, func: Token => A): A = {
    val token = peek
    if (matchAhead(tokenType))
      func(token)
    else
      throw ParseError(s"Expected $tokenType but see ${peek.tokenType}")
  }

  def lookAhead(tokenTypes: List[TokenType]): Boolean =
    tokenTypes contains peek.tokenType

  def lookForward(tokenTypes: List[TokenType], i: Int = 0): Boolean = tokenTypes match {
    case Nil => true
    case _ if i >= current.length => false
    case x :: xs => current(i).tokenType == x && lookForward(xs, i + 1)
  }


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
      TokenType.BOOLEAN,
      TokenType.IDENTIFIER,
      TokenType.LEFT_BRACKET,
      TokenType.LEFT_BRACE
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
        val xs = parseExprList(TokenType.RIGHT_PAREN)
        xs match {
          case Nil => ExprValue(ValUnit)
          case expr :: Nil => expr
          case _ => ExprTuple(xs)
        }
      case TokenType.LEFT_BRACKET =>
        parseList match {
          case Nil => ExprValue(ValList(Nil))
          case x => ExprList(x)
        }
      case TokenType.LEFT_BRACE =>
        if (lookForward(List(TokenType.STRING, TokenType.RIGHT_ARROW)))
          ExprMapping(parseMappingList)
        else ExprBlock(parseEffectList(TokenType.RIGHT_BRACE, TokenType.SEMI_COLON))
      case TokenType.KW_IF => parseIf
      case TokenType.KW_FOR =>
        val combinators = parseForCombinatorList
        ExprFor(combinators, parseExpr)
      case TokenType.INT => ExprValue(ValInt(token.lexeme.asInstanceOf[Int]))
      case TokenType.DOUBLE => ExprValue(ValDouble(token.lexeme.asInstanceOf[Double]))
      case TokenType.STRING => ExprValue(ValString(token.lexeme.asInstanceOf[String]))
      case TokenType.UNIT => ExprValue(ValUnit)
      case TokenType.BOOLEAN => ExprValue(ValBoolean(token.lexeme.asInstanceOf[Boolean]))
      case TokenType.IDENTIFIER =>
        if (matchAhead(TokenType.EQ_GT)) {
          ExprLambda(token.content, TypeExprIdentifier("Any"), parseExpr)
        }
        else if (matchAhead(TokenType.COLON)) {
          val typeExpr = parseTypeExpr
          if (matchAhead(TokenType.EQ_GT)) {
            ExprLambda(token.content, typeExpr, parseExpr)
          } else {
            throw ParseError(s"Unexpected token $peek when parsing a lambda expression.")
          }
        }
        else {
          ExprIdentifier(token.content)
        }
      case _ =>
        throw ParseError(s"Unexpected token: $token")
    }
  }

  def parseIf: Expr = {
    val cond = parseExpr
    expect(TokenType.KW_THEN, { _ =>
      val left = parseExpr
      if (matchAhead(TokenType.KW_ELSE)) {
        val right = parseExpr
        ExprIf(cond, left, Some(right))
      } else ExprIf(cond, left, None)
    })
  }

  def parseForCombinator: ForCombinator =
    if (lookForward(List(TokenType.IDENTIFIER, TokenType.LEFT_ARROW)))
      expect(TokenType.IDENTIFIER, { token =>
        val name = token.content
        advance
        ForBind(name, parseExpr)
      })
    else
      ForFilter(parseExpr)

  def parseForCombinatorList: List[ForCombinator] =
    if (matchAhead(TokenType.KW_DO)) Nil
    else {
      val comb = parseForCombinator
      if (matchAhead(TokenType.KW_DO))
        comb :: Nil
      else if (matchAhead(TokenType.COMMA))
        comb :: parseForCombinatorList
      else throw ParseError(s"Expecting DO or COMMA, but see ${peek.tokenType}.")
    }

  def parseMappingList: List[(String, Expr)] =
    if (matchAhead(TokenType.RIGHT_BRACE)) Nil
    else
      expect(TokenType.STRING, { token =>
        val name = token.lexeme.toString
        expect(TokenType.RIGHT_ARROW, { _ =>
          val pair: (String, Expr) = (name, parseExpr)
          if (matchAhead(TokenType.RIGHT_BRACE))
            pair :: Nil
          else if (matchAhead(TokenType.COMMA))
            pair :: parseMappingList
          else throw ParseError(s"Expecting RIGHT_BRACE or COMMA, but see ${peek.tokenType}.")
        })
      })

  def parseEffectList(endToken: TokenType, sepToken: TokenType = TokenType.SEMI_COLON): List[Effect] =
    if (matchAhead(endToken)) {
      Nil
    } else {
      val effect = parseEffect
      if (matchAhead(endToken))
        effect :: Nil
      else if (lookForward(List(sepToken, endToken))) {
        advance
        advance
        effect :: Nil
      }
      else if (matchAhead(sepToken)) {
        effect :: parseEffectList(endToken, sepToken)
      }
      else
        throw ParseError(s"Expecting pairing token $endToken, but found ${peek.tokenType}")
    }

  def parseExprList(endToken: TokenType, sepToken: TokenType = TokenType.COMMA): List[Expr] =
    if (matchAhead(endToken)) {
      Nil
    } else {
      val expr = parseExpr
      if (matchAhead(sepToken)) {
        expr :: parseExprList(endToken)
      } else if (matchAhead(endToken))
        expr :: Nil
      else {
        throw ParseError(s"Expecting pairing token $endToken.")
      }
    }

  def parseList: List[Expr] = parseExprList(TokenType.RIGHT_BRACKET)

  def parseTypeExpr: TypeExpr = {
    val expr = parseTypeTerm
    if (matchAhead(TokenType.RIGHT_ARROW))
      TypeExprArrow(expr, parseTypeExpr)
    else
      expr
  }

  def parseTypeTerm: TypeExpr = {
    val token = advance
    token.tokenType match {
      case TokenType.IDENTIFIER =>
        TypeExprIdentifier(token.content)
      case TokenType.LEFT_PAREN =>
        parseTypeExprList match {
          case Nil => TypeExprTuple(Nil)
          case expr :: Nil => expr
          case xs => TypeExprTuple(xs)
        }
      case TokenType.LEFT_BRACE =>
        TypeExprMapping(parseTypeMappingList)
      case _ =>
        throw ParseError(s"Invalid type expr at token $peek")
    }
  }

  def parseTypeExprList: List[TypeExpr] =
    if (matchAhead(TokenType.RIGHT_PAREN)) Nil
    else {
      val expr = parseTypeExpr
      if (matchAhead(TokenType.COMMA))
        expr :: parseTypeExprList
      else if (matchAhead(TokenType.RIGHT_PAREN))
        expr :: Nil
      else throw ParseError(s"Expecting COMMA or RIGHT_BRACE when parsing type expression, but see ${peek.tokenType}.")
    }

  def parseTypeMappingList: List[(String, TypeExpr)] =
    if (matchAhead(TokenType.RIGHT_BRACE)) Nil
    else expect(TokenType.STRING, { token =>
      val name = token.lexeme.toString
      expect(TokenType.COLON, { _ =>
        val pair: (String, TypeExpr) = (name, parseTypeExpr)
        if (matchAhead(TokenType.COMMA))
          pair :: parseTypeMappingList
        else if (matchAhead(TokenType.RIGHT_BRACE))
          pair :: Nil
        else throw ParseError(s"Expecting COMMA or RIGHT_BRACE when parsing mapping type, but see ${peek.tokenType}.")
      })
    })

  def parseEffect: Effect =
    if (matchAhead(TokenType.KW_DEF))
      parseValueBindEffect
    else if (lookForward(List(TokenType.IDENTIFIER, TokenType.EQ))) {
      parseAssignEffect
    } else {
      ExprEffect(parseExpr)
    }

  def parseValueBindEffect: Effect =
    expect(TokenType.IDENTIFIER, { token =>
      val name = token.content
      val valType = if (matchAhead(TokenType.COLON)) parseTypeExpr else TypeExprIdentifier("Any")
      expect(TokenType.EQ, { _ =>
        val expr = parseExpr
        ValueBindEffect(name, valType, expr)
      })
    })

  def parseAssignEffect: Effect =
    expect(TokenType.IDENTIFIER, { token =>
      val name = token.content
      advance
      AssignEffect(name, parseExpr)
    })

  def parseBinding: Binding =
    if (matchAhead(TokenType.KW_DEF))
      parseValBinding
    else if (matchAhead(TokenType.KW_TYPE))
      parseTypeBinding
    else
      ExprBinding(parseExpr)

  def parseValBinding: Binding =
    expect(TokenType.IDENTIFIER, { token =>
      val name = token.content
      val valType = if (peek.tokenType == TokenType.COLON) {
        advance
        parseTypeExpr
      } else TypeExprIdentifier("Any")
      expect(TokenType.EQ, { _ =>
        val expr = parseExpr
        ValBinding(name, valType, expr)
      })
    })

  def parseTypeBinding: Binding =
    expect(TokenType.IDENTIFIER, { token =>
      val name = token.content
      expect(TokenType.EQ, { _ =>
        TypeBinding(name, parseTypeExpr)
      })
    })
}

object Parser {
  def parse(tokens: List[Token]): List[Binding] = {
    val parser = new Parser(tokens)
    var ret: List[Binding] = Nil
    while (!parser.eof) ret = ret :+ parser.parseBinding
    ret
  }

  def parseSource(source: String): List[Binding] = parse(Scanner.scanSource(source))
}
