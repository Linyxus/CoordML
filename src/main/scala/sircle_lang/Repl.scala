package sircle_lang

import scala.io.StdIn.readLine

object Repl extends App {
  println("Sircle REPL v0.0.0")
  val evaluator = new Evaluator
  while (true) {
    val line = readLine("> ")
    val tokens = Scanner.getAllTokens(line)
//    print("tokens: ")
//    for (token <- tokens) {
//      print(Token.show(token))
//      print(' ')
//    }
//    println()
    val parser = new Parser(tokens)
    try {
      val binding = parser.parseBinding
      println(Binding show binding)
      println(Value show evaluator.executeBinding(binding))
    } catch {
      case e: ParseError => println("Parse error: " + e.errorMsg)
      case e: RuntimeError => println("Runtime error: " + e.errorMsg)
    }
  }
}
