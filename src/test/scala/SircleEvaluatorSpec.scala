import org.scalatest.flatspec
import org.scalatest.matchers
import flatspec._
import matchers._
import sircle_lang._

class SircleEvaluatorSpec extends AnyFlatSpec with should.Matchers {
  val runtime = new Runtime
  def eval(source: String): Value = runtime.loadSource(source)

  "Sircle evaluator" should "compute int arithmetic expressions correctly" in {
    val suite: List[(String, Int)] = List(
      "1" -> 1,
      "1 + 1" -> 2,
      "1 + -1" -> 0,
      "1 * -1 - 2" -> -3,
      "5 * 5 - 4 * (2 + 3)" -> 5,
    )

    suite foreach { x =>
      eval(x._1) should be (ValInt(x._2))
    }
  }

  it should "evaluate neq operator corretly" in {
    val suite: List[(String, Boolean)] = List(
      "True != True" -> false,
      "1 != 1" -> false,
      "1.0 != 1.0" -> false,
      "[] != []" -> false,
      "buildMapping [] != buildMapping []" -> false,
      "() != ()" -> false,
      "1 != 1.0" -> true,
      "[1] != [2]" -> true,
      "[1] != ()" -> true,
      "True != False" -> true,
    )

    suite.foreach { x =>
      eval(x._1) should be (ValBoolean(x._2))
    }
  }
}
