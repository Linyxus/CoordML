package sircle_lang

object TokenType extends Enumeration {
  type TokenType = Value
  val
    KW_DEF, KW_TYPE,
    PLUS, MINUS, ASTERISK, SLASH, AND, OR, DOLLAR, IN,
    NOT, GT_GT, BAR_BAR, GT, LT, GT_EQ, LT_EQ, EQ, EQ_GT,
    EQ_EQ, BANG_EQ, LEFT_BRACKET, RIGHT_BRACKET,
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, COLON, SEMI_COLON, IDENTIFIER,
    INT, DOUBLE, STRING, BOOLEAN, UNIT,
    ERROR, EOF
    = Value
}
