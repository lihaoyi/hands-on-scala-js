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
        println(f.position)
        println(f.formatExpectedAsString)
        println(f.formatTraces)
        throw new Exception(f.position + "\t" + f.formatTraces)
      case Success(parsed) =>
        assert(parsed == input)
    }
  }
  def tests = TestSuite{
    'unit {
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
      * - check(
        """
          |object SyntaxTest{
          |  a()
          |  throw 1
          |}
        """.stripMargin
      )
      * - check(
        """
          |object SyntaxTest extends TestSuite{
          |  def check[T](input: String) = {
          |    new ScalaSyntax(input).CompilationUnit.run() match{
          |      case Failure(f: ParseError) =>
          |        println(f.position)
          |        println(f.formatExpectedAsString)
          |        println(f.formatTraces)
          |        throw new Exception(f.position + "\t" + f.formatTraces)
          |      case Success(parsed) =>
          |        assert(parsed == input)
          |    }
          |  }
          |}
        """.stripMargin
      )
      * - check(
        """package scalatex
          |
          |
          |import org.parboiled2._
          |import torimatomeru.ScalaSyntax
          |
          |import scalatex.stages.{Trim, Parser, Ast}
          |import scalatex.stages.Ast.Block.{IfElse, For, Text}
          |import Ast.Chain.Args
          |
          |object ParserTests extends utest.TestSuite{
          |  import Ast._
          |  import utest._
          |  def check[T](input: String, parse: Parser => scala.util.Try[T], expected: T) = {
          |    val parsed = parse(new Parser(input)).get
          |    assert(parsed == expected)
          |  }
          |  def tests = TestSuite{}
          |}
        """.stripMargin
      )
    }
    'file{
      * - check(io.Source.fromFile("scalatexApi/src/test/scala/torimatomeru/SyntaxTest.scala").mkString)
      * - check(io.Source.fromFile("scalatexApi/src/test/scala/scalatex/TestUtil.scala").mkString)

//      Seems to run forever? Maybe exponential performance
//      * - check(io.Source.fromFile("scalatexApi/src/test/scala/scalatex/ParserTests.scala").mkString)

      * - check(io.Source.fromFile("scalatexApi/src/main/scala/scalatex/package.scala").mkString)
    }
  }
}
