package torimatomeru

import org.parboiled2.ParseError
import utest._
import utest.framework.Test
import utest.util.Tree

import scala.util.{Failure, Success}

object SyntaxTest extends TestSuite{
  def check[T](input: String) = {
    new ScalaSyntax(input).CompilationUnit.run() match{
      case Failure(f: ParseError) =>
        println(f.formatTraces)
        throw new Exception(f.position + "\t" + f.formatTraces)
      case Success(parsed) =>
        assert(parsed == input)
    }
  }
  def tests = TestSuite{

    * - check(
      "package torimatomeru"

    )
    * - check(
      """
        |package torimatomeru
        |
        |import org.parboiled2.ParseError
        |import utest._
        |import utest.framework.Test
      """.stripMargin

    )
    * - check(
      """
        |package torimatomeru
        |
        |import org.parboiled2.ParseError
        |import utest._
        |import utest.framework.Test
        |import utest.util.Tree
        |
        |import scala.util.{Failure, Success}
        |
        |object SyntaxTest extends TestSuite
      """.stripMargin
    )
    * - check(
      """
        |object SyntaxTest extends TestSuite{
        |  def check[T](input: String) = {
        |
        |  }
        |}
      """.stripMargin
    )
  }
}
