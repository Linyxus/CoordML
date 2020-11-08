package sircle_lang

import TokenType._

case class Token(tokenType: TokenType, line: Int, line_pos: Int, content: String, lexeme: Any)

object Token {
  def showToken(token: Token): String = token.tokenType match {
    case INT => s"INT(${token.lexeme})"
    case DOUBLE => s"DOUBLE(${token.lexeme})"
    case STRING => s"STRING(${token.lexeme})"
    case BOOLEAN => s"BOOLEAN(${token.lexeme})"
    case UNIT => "()"
    case _ => token.content
  }
}
