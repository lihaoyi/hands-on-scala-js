package torimatomeru

import org.parboiled2.ParseError
import utest._
import utest.framework.Test
import utest.util.Tree

import scala.util.{Failure, Success}

object SyntaxTest extends TestSuite{
  def check[T](input: String, parse: ScalaSyntax => scala.util.Try[T], expected: T) = {
    parse(new ScalaSyntax(input)) match{
      case Failure(f: ParseError) =>
        println(f.formatTraces)
        throw new Exception(f.formatTraces)
      case Success(parsed) =>
        assert(parsed == expected)
    }

  }
  def tests = TestSuite{

    * - check(
      """(1
        |)""".stripMargin,
      _.ArgumentExprs().run(), ()
    )
    * - check(
      """(1,
        |1)""".stripMargin,
      _.ArgumentExprs().run(), ()
    )
    * - check(
      """(1, 2,
        |3
        |,
        |4
        |)""".stripMargin,
      _.ArgumentExprs().run(), ()
    )
  }
}
