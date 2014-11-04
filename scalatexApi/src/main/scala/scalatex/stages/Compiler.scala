package scalatex
package stages
import acyclic.file

import scala.reflect.macros.whitebox.Context
import scala.reflect.internal.util.{Position, OffsetPosition}

/**
 * Walks the parsed AST, converting it into a structured Scala c.Tree
 */
object Compiler{

  def apply(c: Context)(fragPos: c.Position, template: Ast.Block): c.Tree = {

    import c.universe._
    def fragType = tq"scalatags.Text.all.Frag"

    println(template)
    def compileChain(code: String, parts: Seq[Ast.Chain.Sub], offset: Int): c.Tree = {
      parts.foldLeft(c.parse(code)){
        case (curr, Ast.Chain.Prop(str, offset2)) => q"$curr.${TermName(str)}"
        case (curr, Ast.Chain.Args(str, offset2)) =>
          val Apply(fun, args) = c.parse(s"omg$str")
          Apply(curr, args)
        case (curr, Ast.Chain.TypeArgs(str, offset2)) =>
          val TypeApply(fun, args) = c.parse(s"omg$str")
          TypeApply(curr, args)
        case (curr, Ast.Block(parts, offset)) =>
          q"$curr(..${compileBlock(parts, offset)})"
        case (curr, Ast.Header(header, block, offset)) =>
          q"$curr(${compileHeader(header, block, offset)})"

      }
    }
    def compileBlock(parts: Seq[Ast.Block.Sub], offset: Int): Seq[c.Tree] = {
      parts.map{
        case Ast.Block.Text(str, _) => q"$str"
        case Ast.Chain(code, parts, offset) => compileChain(code, parts, offset)
        case Ast.Header(header, block, offset) => compileHeader(header, block, offset)
      }
    }
    def compileHeader(header: String, block: Ast.Block, offset: Int): c.Tree = {
      val Block(stmts, expr) = c.parse(s"{$header\n ()}")
      Block(stmts, wrapBlock(compileBlock(block.parts, block.offset)))
    }

    def wrapBlock(items: Seq[c.Tree]) = {
      q"Seq[$fragType](..$items)"
    }
    val res = wrapBlock(compileBlock(template.parts, template.offset))
    println("::::::::::::::::::::::::::::::::::::::::::::::::")
    println(res)
    println("::::::::::::::::::::::::::::::::::::::::::::::::")
    res
  }
}