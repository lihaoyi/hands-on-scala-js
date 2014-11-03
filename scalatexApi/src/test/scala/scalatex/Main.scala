package scalatex


import org.parboiled2._
import torimatomeru.ScalaSyntax

object Main extends utest.TestSuite{
  import Ast._
  import utest._
  def check[T](input: String, parse: ScalatexParser => scala.util.Try[T], expected: T) = {
    val parsed = parse(new ScalatexParser(input))
    assert(parsed.get == expected)
  }
  def tests = TestSuite{
    'Test {
      * - check("i am a cow", _.Text.run(), Block.Text("i am a cow"))
      * - check("i am a @cow", _.Text.run(), Block.Text("i am a "))
      * - check("i am a @@cow", _.Text.run(), Block.Text("i am a @cow"))
      * - check("i am a @@@cow", _.Text.run(), Block.Text("i am a @"))
      * - check("i am a @@@@cow", _.Text.run(), Block.Text("i am a @@cow"))

    }
    'Code{
      * - check("@(1 + 1)", _.Code.run(),  Code("(1 + 1)"))
      * - check("@{{1} + (1)}", _.Code.run(), Code("{{1} + (1)}"))
      * - check("@{val x = 1; 1}", _.Code.run(), Code("{val x = 1; 1}"))
      * - check("@{`{}}{()@`}", _.Code.run(), Code("{`{}}{()@`}"))
    }

    'Block{
      * - check("{i am a cow}", _.TBlock.run(), Block(Seq(Block.Text("i am a cow"))))
      * - check("{i @am a @cow}", _.TBlock.run(),
        Block(Seq(Block.Text("i "), Chain(Code("am"),Seq()), Block.Text(" a "), Chain(Code("cow"),Seq())))
      )
    }
    'Chain{
      * - check("@omg.bbq[omg].fff[fff](123)", _.ScalaChain.run(),
        Chain(Code("omg"),Seq(Chain.Prop(".bbq[omg]"), Chain.Prop(".fff[fff]"), Chain.Args("(123)")))
      )
      * - check("@omg{bbq}.cow(moo){a @b}", _.ScalaChain.run(),
        Chain(Code("omg"),Seq(
          Block(Seq(Block.Text("bbq"))),
          Chain.Prop(".cow"),
          Chain.Args("(moo)"),
          Block(Seq(Block.Text("a "), Chain(Code("b"), Nil)))
        ))
      )
    }

  }
  def p(input: String) = {
    new ScalatexParser(input)
  }
}
trait Ast{
  def offset: Int
}
object Ast{
  case class Code(code: String, offset: Int = 0) extends Ast
  case class Block(parts: Seq[Block.Sub], offset: Int = 0) extends Chain.Sub
  object Block{
    trait Sub
    case class Text(txt: String, offset: Int = 0) extends Block.Sub
  }
  case class Chain(lhs: Code, parts: Seq[Chain.Sub], offset: Int = 0) extends Block.Sub
  object Chain{
    trait Sub extends Ast
    case class Prop(str: String, offset: Int = 0) extends Sub
    case class Args(str: String, offset: Int = 0) extends Sub
  }
}
class ScalatexParser(input: ParserInput) extends ScalaSyntax(input) {
  def TextNot(chars: String) = rule { capture(oneOrMore(noneOf(chars) | "@@")) ~> (x => Ast.Block.Text(x.replace("@@", "@"))) }
  def Text = TextNot("@")
  def Code = rule {
    "@" ~ capture(Id | BlockExpr | ('(' ~ optional(Exprs) ~ ')')) ~> (Ast.Code(_))
  }
  def ScalaChain = rule { Code ~ zeroOrMore(Extension) ~> (Ast.Chain(_, _)) }
  def Extension: Rule1[Ast.Chain.Sub] = rule {
    (capture(('.' ~ Id) ~ optional(TypeArgs)) ~> (Ast.Chain.Prop(_))) |
    (capture(!BlockExpr ~ ArgumentExprs) ~> (Ast.Chain.Args(_))) |
    TBlock
  }
  def TBlock = rule{ '{' ~ Body ~ '}' }
  def Body = rule{ zeroOrMore(TextNot("@}") | ScalaChain) ~> (x => Ast.Block(x)) }
}


