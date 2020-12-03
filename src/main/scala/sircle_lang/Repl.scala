package sircle_lang

import scala.io.StdIn.readLine
import scala.io.Source

// repl app entry
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

  val commandList: Map[String, String => String] = Map(
    "t" -> { x: String => ValueType show evalStr(x).valueType },
    "l" -> { x: String => Value show loadFile(x) },
    "q" -> { _ => sys.exit(0) }
  )

  println("Sircle REPL v0.0.0")
  println("Loading prelude source")
  try {
    evalStr(Preload.preloadSource)
  } catch {
    case e: ParseError => println("Parse error: " + e.errorMsg)
    case e: RuntimeError => println("Runtime error: " + e.errorMsg)
  }
  while (true) {
    val line = readLine("> ")
    try {
      if (line.startsWith(":")) {
        val arg = line.substring(2).trim
        val cmd = line.substring(1, 2)
        commandList get cmd match {
          case Some(func) => println(func(arg))
          case None => println(s"Undefined command $cmd.")
        }
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
