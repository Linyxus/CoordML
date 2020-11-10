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
      var expr = parser.parseExpr
      println(Expr showExpr expr)
      expr = ListDesugar transform expr
      println(Expr showExpr expr)
      val evaluator = new Evaluator
      println(Value showValue evaluator.evalExpr(expr))
    } catch {
      case e: ParseError => println("Parse error: " + e.errorMsg)
      case e: RuntimeError => println("Runtime error: " + e.errorMsg)
    }
  }
}
