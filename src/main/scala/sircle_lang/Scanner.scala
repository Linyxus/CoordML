package sircle_lang

import TokenType._

// the token scanner
class Scanner(val source: String) {
  var start: Int = 0
  var current: Int = 0
  var line: Int = 0
  var linePos: Int = 0
  var startLinePos: Int = 0

  def makeToken(tokenType: TokenType, value: Option[Any] = None): Token =
    Token(tokenType, line, startLinePos, source.substring(start, current), value.orNull)

  def errorToken(errorMsg: String): Token =
    Token(ERROR, line, linePos, errorMsg, null)

  def peek: Char =
    if (eof) {
      '\u0000'
    } else {
      source(current)
    }

  def eof: Boolean = current >= source.length

  // advance the pointer when the token matches the specified character
  def matchAhead(ch: Char): Boolean =
    if (peek == ch) {
      advance
      true
    } else false

  def advance: Char =
    if (!eof) {
      if (peek == '\r' || peek == '\n') {
        line += 1
        linePos = 0
      } else {
        linePos += 1
      }
      current += 1
      source(current - 1)
    } else '\u0000'

  def skipSpace(): Unit =
    while (peek.isSpaceChar || peek == '\n' || peek == '\r' || peek == '\t') advance

  def scanNumber: Token = {
    def scanFrac: Token = {
      while (peek.isDigit) {
        advance
      }
      peek match {
        case 'e' =>
          advance
          return scanExp
        case '"' => return errorToken("Invalid postfix for numbers.")
        case x if x.isLetter => return errorToken("Invalid postfix for numbers.")
        case _ =>
      }

      val s = source.substring(start, current)
      makeToken(DOUBLE, Some(s.toDouble))
    }

    def scanExp: Token = {
      while (peek.isDigit)
        advance
      val s = source.substring(start, current)
      makeToken(DOUBLE, Some(s.toDouble))
    }

    while (peek.isDigit) advance
    peek match {
      case '.' =>
        advance
        return scanFrac
      case 'e' =>
        advance
        return scanExp
      case '"' => return errorToken("Invalid postfix for numbers.")
      case x if x.isLetter => return errorToken("Invalid postfix for numbers.")
      case _ =>
    }
    val s = source.substring(start, current)
    makeToken(INT, Some(s.toInt))
  }

  def scanIdentifier(): Unit = {
    while (peek.isLetterOrDigit || peek == '\'')
      advance
  }

  def scanString: Token = {
    while (peek != '\"' && !eof) {
      peek match {
        case '\\' =>
          advance
        case _ =>
      }
      advance
    }
    if (eof)
      errorToken("Unexpected end of input while scanning a string.")
    else {
      advance
      val esc: CharSequence = "\\n"
      val rep: CharSequence = "\n"
      val s = source.substring(start + 1, current - 1).replace(esc, rep)
      makeToken(STRING, Some(s))
    }
  }

  def nextToken: Token = {
    skipSpace()
    start = current
    startLinePos = linePos
    if (eof) return makeToken(EOF)

    val ch = advance

    ch match {
      case '+' => makeToken(PLUS)
      case '-' if matchAhead('>') => makeToken(RIGHT_ARROW)
      case '-' => makeToken(MINUS)
      case '*' => makeToken(ASTERISK)
      case '/' => makeToken(SLASH)
      case '$' => makeToken(DOLLAR)
      case '>' if matchAhead('=') => makeToken(GT_EQ)
      case '>' if matchAhead('>') => makeToken(GT_GT)
      case '>' => makeToken(GT)
      case '<' if matchAhead('=') => makeToken(LT_EQ)
      case '<' if matchAhead('-') => makeToken(LEFT_ARROW)
      case '<' => makeToken(LT)
      case '!' if matchAhead('=') => makeToken(BANG_EQ)
      case '=' if matchAhead('=') => makeToken(EQ_EQ)
      case '=' if matchAhead('>') => makeToken(EQ_GT)
      case '=' => makeToken(EQ)
      case '|' if matchAhead('|') => makeToken(BAR_BAR)
      case '[' => makeToken(LEFT_BRACKET)
      case '(' if matchAhead(')') => makeToken(UNIT, Some(()))
      case '(' => makeToken(LEFT_PAREN)
      case '{' => makeToken(LEFT_BRACE)
      case ']' => makeToken(RIGHT_BRACKET)
      case ')' => makeToken(RIGHT_PAREN)
      case '}' => makeToken(RIGHT_BRACE)
      case ';' => makeToken(SEMI_COLON)
      case ':' => makeToken(COLON)
      case ',' => makeToken(COMMA)
      case '.' => makeToken(DOT)
      case x if x.isDigit => scanNumber
      case x if x.isLetterOrDigit =>
        scanIdentifier()
        source.substring(start, current) match {
          case "def" => makeToken(KW_DEF)
          case "type" => makeToken(KW_TYPE)
          case "if" => makeToken(KW_IF)
          case "then" => makeToken(KW_THEN)
          case "else" => makeToken(KW_ELSE)
          case "for" => makeToken(KW_FOR)
          case "do" => makeToken(KW_DO)
          case "and" => makeToken(AND)
          case "or" => makeToken(OR)
          case "not" => makeToken(NOT)
          case "in" => makeToken(IN)
          case "True" => makeToken(BOOLEAN, Some(true))
          case "False" => makeToken(BOOLEAN, Some(false))
          case _ => makeToken(IDENTIFIER)
        }
      case '"' => scanString
      case _ => errorToken(s"Unexpected character.")
    }
  }
}

object Scanner {
  def getAllTokens(source: String): List[Token] = extractTokens(new Scanner(source))

  def extractTokens(scanner: Scanner): List[Token] = {
    val token = scanner.nextToken
    token.tokenType match {
      case EOF => token :: Nil
      case _ => token :: extractTokens(scanner)
    }
  }

  def scanSource(source: String): List[Token] = {
    val tokens = getAllTokens(source)
    tokens filter { x => x.tokenType == ERROR } match {
      case Nil => tokens
      case xs => throw ScanError(xs)
    }
  }
}
