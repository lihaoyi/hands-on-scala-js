package scalatex

import org.parboiled2._
import torimatomeru.ScalaSyntax

trait Ast{
  def offset: Int
}
object Ast{
  case class Block(parts: Seq[Block.Sub], offset: Int = 0) extends Chain.Sub
  object Block{
    trait Sub
    case class Text(txt: String, offset: Int = 0) extends Block.Sub
  }
  case class Code(code: String, offset: Int = 0)
  case class Chain(lhs: Code, parts: Seq[Chain.Sub], offset: Int = 0) extends Block.Sub
  object Chain{
    trait Sub
    case class Prop(str: String, offset: Int = 0) extends Sub
    case class Args(str: String, offset: Int = 0) extends Sub
  }
}
class ScalatexParser(input: ParserInput, indent: Int = 0) extends ScalaSyntax(input) {
  val txt = input.sliceString(0, input.length)
  val indentTable = txt.split('\n').map{ s =>
    if (s.trim == "") -1
    else s.takeWhile(_ == ' ').length
  }
  val nextIndentTable = (0 until indentTable.length).map { i =>
    val index = indentTable.indexWhere(_ != -1, i + 1)
    if (index == -1) 100000
    else indentTable(index)
  }
  def cursorNextIndent() = {
    nextIndentTable(txt.take(cursor).count(_ == '\n'))
  }
  println("INDENT " + indent)
  def TextNot(chars: String) = rule {
    capture(oneOrMore(noneOf(chars + "\n") | "@@")) ~> {
      x => Ast.Block.Text(x.replace("@@", "@"))
    }
  }
  def Text = TextNot("@")
  def Code = rule {
    "@" ~ capture(Id | BlockExpr | ('(' ~ optional(Exprs) ~ ')')) ~> (Ast.Code(_))
  }
  def BlankLine = rule{ '\n' ~ zeroOrMore(' ') ~ &('\n') }
  def Indent = rule{ '\n' ~ indent.times(' ') ~ zeroOrMore(' ') }
  def LoneScalaChain: Rule1[Ast.Chain] = rule {
    Indent ~
    ScalaChain ~
    zeroOrMore(BlankLine) ~
    test(cursorNextIndent() > indent) ~
    runSubParser {
      new ScalatexParser(_, cursorNextIndent()).Body
    } ~> { (chain: Ast.Chain, body: Ast.Block) =>
      chain.copy(parts = chain.parts :+ body)
    }
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
    zeroOrMore(
      LoneScalaChain |
      TextNot("@}") |
      (capture(Indent) ~> (Ast.Block.Text(_))) |
      (capture(BlankLine) ~> (Ast.Block.Text(_))) |
      ScalaChain
    ) ~> {x =>
      Ast.Block(x)
    }
  }
}
