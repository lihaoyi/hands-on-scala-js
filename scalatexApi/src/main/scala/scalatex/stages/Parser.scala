package scalatex
package stages

import org.parboiled2._
import torimatomeru.ScalaSyntax
import Util._
trait Ast{
  def offset: Int
}
object Ast{
  case class Block(parts: Seq[Block.Sub], offset: Int = 0) extends Chain.Sub
  object Block{
    trait Sub extends Ast
    case class Text(txt: String, offset: Int = 0) extends Block.Sub
  }

  case class Chain(lhs: String, parts: Seq[Chain.Sub], offset: Int = 0) extends Block.Sub
  object Chain{
    trait Sub extends Ast
    case class Prop(str: String, offset: Int = 0) extends Sub
    case class TypeArgs(str: String, offset: Int = 0) extends Sub
    case class Args(str: String, offset: Int = 0) extends Sub
  }
}
object Parser{
  def parse(input: String): Ast.Block = {
    new Parser(input).Body.run().get
  }
}
class Parser(input: ParserInput, indent: Int = 0) extends ScalaSyntax(input) {
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

  def TextNot(chars: String) = rule {
    capture(oneOrMore(noneOf(chars + "\n") | "@@")) ~> {
      x => Ast.Block.Text(x.replace("@@", "@"))
    }
  }
  def Text = TextNot("@")
  def Code = rule {
    "@" ~ capture(Id | BlockExpr2 | ('(' ~ optional(Exprs) ~ ')'))
  }
  def BlankLine = rule{ '\n' ~ zeroOrMore(' ') ~ &('\n') }
  def Indent = rule{ '\n' ~ indent.times(' ') ~ zeroOrMore(' ') }
  def LoneScalaChain: Rule2[Ast.Block.Text, Ast.Chain] = rule {
    (capture(Indent) ~> (Ast.Block.Text(_))) ~
    ScalaChain ~
    zeroOrMore(BlankLine) ~
    test(cursorNextIndent() > indent) ~
    runSubParser {
      new Parser(_, cursorNextIndent()).Body
    } ~> { (chain: Ast.Chain, body: Ast.Block) =>
      chain.copy(parts = chain.parts :+ body)
    }
  }

  def ScalaChain = rule {
    Code ~ zeroOrMore(Extension) ~> {Ast.Chain(_, _)}
  }
  def Extension: Rule1[Ast.Chain.Sub] = rule {
    ('.' ~ capture(Id) ~> (Ast.Chain.Prop(_))) |
    (capture(TypeArgs2) ~> (Ast.Chain.TypeArgs(_))) |
    (capture(ArgumentExprs2) ~> (Ast.Chain.Args(_))) |
    TBlock
  }
  def Ws = Whitespace
  // clones of the version in ScalaSyntax, but without tailing whitespace or newlines
  def TypeArgs2 = rule { '[' ~ Ws ~ Types ~ ']' }
  def ArgumentExprs2 = rule {
    '(' ~ Ws ~ (optional(Exprs ~ ',' ~ Ws) ~ PostfixExpr ~ ':' ~ Ws ~ '_' ~ Ws ~ '*' ~ Ws | optional(Exprs)) ~ ')'
  }
  def BlockExpr2: Rule0 = rule { '{' ~ Ws ~ (CaseClauses | Block) ~ '}' }
  def TBlock = rule{ '{' ~ Body ~ '}' }

  def Body = rule{
    oneOrMore(
      LoneScalaChain ~> (Seq(_, _)) |
      TextNot("@}") ~> (Seq(_)) |
      (capture(Indent) ~> (x => Seq(Ast.Block.Text(x)))) |
      (capture(BlankLine) ~> (x => Seq(Ast.Block.Text(x)))) |
      ScalaChain ~> (Seq(_))
    ) ~> {x =>
      Ast.Block(x.flatten)
    }
  }
}
