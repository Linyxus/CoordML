import org.scalatest.flatspec.AnyFlatSpec
import sircle_lang._

class SircleEvaluatorSpec extends AnyFlatSpec {
  val evaluator = new Evaluator
  def eval(source: String): Value = {
    val tokens = Scanner.getAllTokens(source)
    val parser = new Parser(tokens)
    var ret: Value = ValUnit
    while (!parser.eof) {
      val binding = parser.parseBinding
      ret = evaluator.executeBinding(binding)
    }
    ret
  }

  "Sircle evaluator" should "compute int arithmetic expressions correctly" in {
    val suite: List[(String, Int)] = List(
      "1" -> 1,
      "1 + 1" -> 2,
      "1 + -1" -> 0,
      "1 * -1 - 2" -> -3,
      "5 * 5 - 4 * (2 + 3)" -> 5,
    )

    suite foreach { x =>
      assert(eval(x._1) == ValInt(x._2))
    }
  }
}
