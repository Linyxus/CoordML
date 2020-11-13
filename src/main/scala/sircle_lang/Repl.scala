package sircle_lang

import scala.io.StdIn.readLine

object Repl extends App {
  println("Sircle REPL v0.0.0")
  val evaluator = new Evaluator
  while (true) {
    val line = readLine("> ")
    val tokens = Scanner.getAllTokens(line)
    println(s"tokens: ${tokens.map(_.toString) mkString " "}")
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
