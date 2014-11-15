package torimatomeru
import utest._
import utest.framework.Test
import utest.util.Tree

object SyntaxTest extends TestSuite{
  def check[T](input: String, parse: ScalaSyntax => scala.util.Try[T], expected: T) = {
    val parsed = parse(new ScalaSyntax(input)).get
    assert(parsed == expected)
  }
  def tests = TestSuite{

    "omg" - check(
      """if (true) () else ()""",
      _.IfCFlow.run(), ()
    )
  }
}
