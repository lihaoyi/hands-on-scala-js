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

    def incPosRec(trees: c.Tree, offset: Int): trees.type = {
      println(s"incPosRec\t$offset\t$trees")
      trees.foreach(incPos(_, offset))
      trees
    }
    def incPos(tree: c.Tree, offset: Int): tree.type = {
      println(s"incPos\t$offset\t$tree")
      val current = if (tree.pos == NoPosition) 0 else tree.pos.point
      c.internal.setPos(tree,
        new OffsetPosition(
          fragPos.source,
          offset + current + fragPos.point
        ).asInstanceOf[c.universe.Position]
      )
      tree
    }

    println(template)
    def compileChain(code: String, parts: Seq[Ast.Chain.Sub], offset: Int): c.Tree = {
      println("CODE " + code)
      val out = parts.foldLeft(incPosRec(c.parse(code), offset + 1)){
        case (curr, Ast.Chain.Prop(str, offset2)) =>
          incPos(q"$curr.${TermName(str)}", offset2 + 1)
        case (curr, Ast.Chain.Args(str, offset2)) =>
          val Apply(fun, args) = c.parse(s"omg$str")
          incPos(Apply(curr, args.map(incPosRec(_, offset2 - 2))), offset2)
        case (curr, Ast.Chain.TypeArgs(str, offset2)) =>
          val TypeApply(fun, args) = c.parse(s"omg$str")
          incPos(TypeApply(curr, args.map(incPosRec(_, offset2 - 2))), offset2)
        case (curr, Ast.Block(parts, offset)) =>
          q"$curr(${compileBlock(parts, offset)})"
        case (curr, Ast.Header(header, block, offset)) =>
          q"$curr(${compileHeader(header, block, offset)})"

      }
      out.foreach(o => println(o.pos + "\t" + o))
      out
    }
    def compileBlock(parts: Seq[Ast.Block.Sub], offset: Int): c.Tree = {
      val res = parts.map{
        case Ast.Block.Text(str, _) => q"$str"
        case Ast.Chain(code, parts, offset) => compileChain(code, parts, offset)
        case Ast.Header(header, block, offset) => compileHeader(header, block, offset)
        case b @ Ast.Block.IfElse(condString, Ast.Block(parts2, offset2), elseBlock, offset) =>
          println("AST " + b)
          val If(cond, _, _) = c.parse(condString + "{}")
          val elseCompiled = elseBlock match{
            case Some(Ast.Block(parts3, offset3)) => compileBlock(parts3, offset3)
            case None => EmptyTree
          }

          val res = If(cond, compileBlock(parts2, offset2), elseCompiled)
          println("Tree " + res)
          res
        case Ast.Block.For(generators, Ast.Block(parts2, offset2), offset) =>
          val fresh = c.fresh()

          val tree = c.parse(s"$generators yield $fresh" )
          def rec(t: Tree): Tree = t match {
            case a @ Apply(fun, List(f @ Function(vparams, body))) =>
              val f2 = Function(vparams, rec(body))
              val a2 = Apply(fun, List(f2))
              a2
            case Ident(x: TermName) if x.decoded == fresh =>
              compileBlock(parts2, offset2)
          }

          val out = rec(tree)
          println(out)
          out
      }
      incPos(q"Seq[$fragType](..$res)", offset)
    }
    def compileHeader(header: String, block: Ast.Block, offset: Int): c.Tree = {
      val Block(stmts, expr) = c.parse(s"{$header\n ()}")
      Block(stmts, compileBlock(block.parts, block.offset))
    }

    val res = compileBlock(template.parts, template.offset)
    println("::::::::::::::::::::::::::::::::::::::::::::::::")
    println(res)
    println("::::::::::::::::::::::::::::::::::::::::::::::::")
    res
  }
}