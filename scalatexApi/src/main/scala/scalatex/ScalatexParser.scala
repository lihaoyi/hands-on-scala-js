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
