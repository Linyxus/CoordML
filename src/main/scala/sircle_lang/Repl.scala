package sircle_lang

import scala.io.StdIn.readLine

object Repl extends App {
  println("Sircle REPL v0.0.0")
  while (true) {
    val line = readLine("> ")
    val tokens = Scanner.getAllTokens(line)
    print("tokens: ")
    for (token <- tokens) {
      print(Token.show(token))
      print(' ')
    }
    println()
    val parser = new Parser(tokens)
    try {
      var expr = parser.parseExpr
      print("parsed: ")
      println(Expr show expr)
      expr = ListDesugar transform expr
      print("list desugar: ")
      println(Expr show expr)
      print("binary op desugar: ")
      expr = BinOpDesugar transform expr
      println(Expr show expr)
      print("unary op desugar: ")
      expr = UnaryOpDesugar transform expr
      println(Expr show expr)
      val evaluator = new Evaluator
      println(Value showValue evaluator.evalExpr(expr))
    } catch {
      case e: ParseError => println("Parse error: " + e.errorMsg)
      case e: RuntimeError => println("Runtime error: " + e.errorMsg)
    }
  }
}
