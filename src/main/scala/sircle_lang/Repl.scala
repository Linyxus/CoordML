package sircle_lang

import scala.io.StdIn.readLine
import scala.io.Source

object Repl extends App {
  val evaluator = new Evaluator

  def evalStr(source: String): Value = {
    val tokens = Scanner.getAllTokens(source)
    //    println(s"tokens: ${tokens.map(_.toString) mkString " "}")
    val parser = new Parser(tokens)
    var ret: Value = ValUnit
    while (!parser.eof) {
      val binding = parser.parseBinding
//      println(Binding show binding)
      ret = evaluator.executeBinding(binding)
    }
    ret
  }

  def loadFile(path: String): Value = {
    val source = Source.fromFile(path).getLines mkString "\n"
    evalStr(source)
  }

  println("Sircle REPL v0.0.0")
  println("Loading preloaded source")
  loadFile("./assets/sircle_lang/preload.sircle")
  while (true) {
    val line = readLine("> ")
    try {
      if (line.startsWith(":t")) {
        val value = evalStr(line.substring(2))
        println(ValueType show value.valueType)
      }
      else if (line.startsWith(":l")) {
        val path = line.substring(3)
        println(Value show loadFile(path))
      } else {
        val value = evalStr(line)
        println(Value show value)
      }
    } catch {
      case e: ParseError => println("Parse error: " + e.errorMsg)
      case e: RuntimeError => println("Runtime error: " + e.errorMsg)
    }
  }
}
