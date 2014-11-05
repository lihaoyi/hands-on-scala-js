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

  def HeaderBlock: Rule1[Ast.Header] = rule{
    Header ~ zeroOrMore(capture(NewlineS) ~ Header ~> (_ + _)) ~ runSubParser{new Parser(_, indent).Body0} ~> {
      (start: String, heads: Seq[String], body: Ast.Block) => Ast.Header(start + heads.mkString, body)
    }
  }

  def BlankLine = rule{ '\n' ~ zeroOrMore(' ') ~ &('\n') }
  def Indent = rule{ '\n' ~ indent.times(' ') ~ zeroOrMore(' ') }
  def LoneScalaChain: Rule2[Ast.Block.Text, Ast.Chain] = rule {
    (capture(Indent) ~> (Ast.Block.Text(_))) ~
    ScalaChain ~
    IndentBlock ~> {
      (chain: Ast.Chain, body: Ast.Block) => chain.copy(parts = chain.parts :+ body)
    }
  }
  def IndentBlock = rule{
    test(cursorNextIndent() > indent) ~
    runSubParser(new Parser(_, cursorNextIndent()).Body)
  }
  def IfHead = rule{ "@" ~ capture("if" ~ "(" ~ Expr ~ ")") }
  def IfElse1 = rule{
   IfHead ~ BraceBlock ~ optional("else" ~ (BraceBlock | IndentBlock))
  }
  def IfElse2 = rule{
    IfHead ~ IndentBlock ~ optional(Indent ~ "@else" ~ (BraceBlock | IndentBlock))
  }
  def IfElse = rule{
    (IfElse1 | IfElse2) ~> (Ast.Block.IfElse(_, _, _))
  }
  def ForLoop = rule{
    "@" ~
    capture("for" ~ ('(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}')) ~
    BraceBlock ~> (Ast.Block.For(_, _))
  }
  def LoneForLoop = rule{
    "@" ~
    capture("for" ~ ('(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}')) ~
    IndentBlock ~>
    (Ast.Block.For(_, _))
  }
  def ScalaChain = rule {
    Code ~ zeroOrMore(Extension) ~> {Ast.Chain(_, _)}
  }
  def Extension: Rule1[Ast.Chain.Sub] = rule {
    ('.' ~ capture(Id) ~> (Ast.Chain.Prop(_))) |
    (capture(TypeArgs2) ~> (Ast.Chain.TypeArgs(_))) |
    (capture(ArgumentExprs2) ~> (Ast.Chain.Args(_))) |
    BraceBlock
  }
  def Ws = Whitespace
  // clones of the version in ScalaSyntax, but without tailing whitespace or newlines
  def TypeArgs2 = rule { '[' ~ Ws ~ Types ~ ']' }
  def ArgumentExprs2 = rule {
    '(' ~ Ws ~ (optional(Exprs ~ ',' ~ Ws) ~ PostfixExpr ~ ':' ~ Ws ~ '_' ~ Ws ~ '*' ~ Ws | optional(Exprs)) ~ ')'
  }
  def BlockExpr2: Rule0 = rule { '{' ~ Ws ~ (CaseClauses | Block) ~ '}' }
  def BraceBlock: Rule1[Ast.Block] = rule{ '{' ~ Body ~ '}' }

  def BodyItem: Rule1[Seq[Ast.Block.Sub]] = rule{
    LoneScalaChain ~> (Seq(_, _)) |
    HeaderBlock ~> (Seq(_)) |
    ForLoop ~> (Seq(_)) |
    LoneForLoop ~> (Seq(_)) |
    TextNot("@}") ~> (Seq(_)) |
    (capture(Indent) ~> (x => Seq(Ast.Block.Text(x)))) |
    (capture(BlankLine) ~> (x => Seq(Ast.Block.Text(x)))) |
    ScalaChain ~> (Seq(_: Ast.Block.Sub))
  }
  def Body = rule{
    oneOrMore(BodyItem) ~> {x =>
      Ast.Block(x.flatten)
    }
  }
  def Body0 = rule{
    zeroOrMore(BodyItem) ~> {x =>
      Ast.Block(x.flatten)
    }
  }
}

trait Ast{
  def offset: Int
}
object Ast{

  /**
   * @param parts The various bits of text and other things which make up this block
   * @param offset
   */
  case class Block(parts: Seq[Block.Sub],
                   offset: Int = 0)
                   extends Chain.Sub with Block.Sub
  object Block{
    trait Sub extends Ast
    case class Text(txt: String, offset: Int = 0) extends Block.Sub
    case class For(generators: String, block: Block, offset: Int = 0) extends Block.Sub
    case class IfElse(condition: String, block: Block, elseBlock: Option[Block], offset: Int = 0) extends Block.Sub
  }
  case class Header(front: String, block: Block, offset: Int = 0) extends Block.Sub with Chain.Sub

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