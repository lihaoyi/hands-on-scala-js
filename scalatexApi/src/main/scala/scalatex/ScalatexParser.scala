package scalatex

import org.parboiled2._
import torimatomeru.ScalaSyntax

trait Ast{
  def offset: Int
}
object Ast{
  case class Block(parts: Seq[Block.Sub], offset: Int = 0) extends Chain.Sub{
    println("Init Block")
  }
  object Block{
    trait Sub
    case class Text(txt: String, offset: Int = 0) extends Block.Sub{
      println("Init Text " + txt)
    }
  }
  case class Code(code: String, offset: Int = 0)
  case class Chain(lhs: Code, parts: Seq[Chain.Sub], offset: Int = 0) extends Block.Sub{
    println("Init Chain " + lhs)
  }
  object Chain{
    trait Sub
    case class Prop(str: String, offset: Int = 0) extends Sub
    case class Args(str: String, offset: Int = 0) extends Sub
  }
}
class ScalatexParser(input: ParserInput, indent: Int = 0) extends ScalaSyntax(input) {
  println("INDENT " + indent)
  def TextNot(chars: String) = rule {
    capture(oneOrMore(noneOf(chars + "\n"))) ~> {
      x => Ast.Block.Text(x.replace("@@", "@"))
    }
  }
  def Text = TextNot("@")
  def Code = rule {
    "@" ~ capture(Id | BlockExpr | ('(' ~ optional(Exprs) ~ ')')) ~> (Ast.Code(_))
  }
  def BlankLine = rule{ run(println("BL A " + (cursorChar.toInt, '\n'.toInt))) ~ '\n' ~ run(println("BL B")) ~ zeroOrMore(' ') ~ run(println("BL C")) ~ &('\n') ~ run(println("BL D")) }
  def Indent = rule{ run(println("Indent A " + cursorChar.toInt)) ~ '\n' ~ run(println("Indent A")) ~ indent.times(' ') ~ run(println("Indent A")) ~ zeroOrMore(' ') }
  def LoneScalaChain: Rule1[Ast.Chain] = rule {
    run(println("LSC Indent")) ~ Indent ~
    run(println("LSC SC")) ~ ScalaChain ~
    zeroOrMore(BlankLine) ~
    run(println("LSC NewLine")) ~
    runSubParser {
      new ScalatexParser(_, indent + 2).Body
    } ~> { (chain: Ast.Chain, body: Ast.Block) =>
      println("chain.copy " + body)
      chain.copy(parts = chain.parts :+ body)
    }
  }

  def newInput(s: String, x: ParserInput): String = {
    def newInput = '\n' + s + x.sliceString(0, x.length)
    println(newInput.split("\n").map("|" + _ + "|").mkString("\n"))
    newInput
  }
  def ScalaChain = rule {
    Code ~ zeroOrMore(Extension) ~> {Ast.Chain(_, _)}
  }
  def Extension: Rule1[Ast.Chain.Sub] = rule {
    (capture(('.' ~ Id) ~ optional(TypeArgs)) ~> (Ast.Chain.Prop(_))) |
    (capture(!BlockExpr ~ ArgumentExprs) ~> (Ast.Chain.Args(_))) |
    TBlock
  }
  def TBlock = rule{ '{' ~ Body ~ '}' }
  def Body = rule{
    run(println("BODY")) ~
    zeroOrMore(
      run(println("Body LSC")) ~ LoneScalaChain |
      run(println("Body TextNot")) ~ TextNot("@}") |
      run(println("Body Indent")) ~ (capture(Indent) ~> (Ast.Block.Text(_))) |
      run(println("Body BlankLine")) ~ (capture(BlankLine) ~> (Ast.Block.Text(_)))
//       | ScalaChain
    ) ~> {x =>
      println("BODY ENDED: " + x)
      Ast.Block(x)
    }
  }
}
