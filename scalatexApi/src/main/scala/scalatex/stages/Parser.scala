package scalatex
package stages

import org.parboiled2._
import torimatomeru.ScalaSyntax
import Util._

/**
 * Parses the input text into a roughly-structured AST. This AST
 * is much simpler than the real Scala AST, but serves us well
 * enough until we stuff the code-strings into the real Scala
 * parser later
 */
object Parser extends (String => Ast.Block){
  def apply(input: String): Ast.Block = {
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
  def Header = rule {
    "@" ~ capture(Def | Import)
  }

  def HeaderBlock: Rule1[Ast.Block] = rule{
    oneOrMore(Header ~ NewlineS) ~ runSubParser{new Parser(_, indent).Body0} ~> {
      (head: Seq[String], body: Ast.Block) => body.copy(front = Some(head.mkString("\n")))
    }
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

  def BodyItem = rule{
    LoneScalaChain ~> (Seq(_, _)) |
      HeaderBlock ~> (Seq(_)) |
      TextNot("@}") ~> (Seq(_)) |
      (capture(Indent) ~> (x => Seq(Ast.Block.Text(x)))) |
      (capture(BlankLine) ~> (x => Seq(Ast.Block.Text(x)))) |
      ScalaChain ~> (Seq(_))
  }
  def Body = rule{
    oneOrMore(BodyItem) ~> {x =>
      Ast.Block(None, x.flatten)
    }
  }
  def Body0 = rule{
    zeroOrMore(BodyItem) ~> {x =>
      Ast.Block(None, x.flatten)
    }
  }
}

trait Ast{
  def offset: Int
}
object Ast{

  /**
   * @param front Any parameter lists (if it's a lambda), imports, val/def/lazy-val,
   *              class/object/trait declarations that occur before the parts of this
   *              block, and must be in scope within those parts
   * @param parts The various bits of text and other things which make up this block
   * @param offset
   */
  case class Block(front: Option[String],
                   parts: Seq[Block.Sub],
                   offset: Int = 0)
                   extends Chain.Sub with Block.Sub
  object Block{
    trait Sub extends Ast
    case class Text(txt: String, offset: Int = 0) extends Block.Sub
  }

  /**
   * @param lhs The first expression in this method-chain
   * @param parts A list of follow-on items chained to the first
   * @param offset
   */
  case class Chain(lhs: String, parts: Seq[Chain.Sub], offset: Int = 0) extends Block.Sub
  object Chain{
    trait Sub extends Ast
    case class Prop(str: String, offset: Int = 0) extends Sub
    case class TypeArgs(str: String, offset: Int = 0) extends Sub
    case class Args(str: String, offset: Int = 0) extends Sub
  }
}