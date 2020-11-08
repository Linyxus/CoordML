package sircle_lang

import scala.io.StdIn.readLine

object Repl extends App {
  println("Sircle REPL v0.0.0")
  while (true) {
    val line = readLine("> ")
    val tokens = Scanner.getAllTokens(line)
    for (token <- tokens) {
      print(Token.showToken(token))
      print(' ')
    }
    println()
    val parser = new Parser(tokens)
    try {
      val expr = parser.parseExpr
      println(Expr.showExpr(expr))
    } catch {
      case e: ParseError => println(e.errorMsg)
    }
  }
}
