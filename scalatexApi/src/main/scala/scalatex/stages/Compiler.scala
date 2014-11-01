package scalatex.stages
import acyclic.file

import scala.reflect.macros.Context
import scala.reflect.internal.util.{Position, OffsetPosition}

/**
 * Walks the parsed AST, converting it into an un-structured Scala source-blob
 * which when compiled results in a function that can be used to generate the
 * given Frag at runtime.
 */
object Compiler{
  val WN = TwistNodes
  def apply(c: Context)(literalPos: c.Position, template: WN.Template): c.Tree = {

    import c.universe._
    def fragType = tq"scalatags.Text.all.Frag"
    def posFor(offset: Int) = {
      new OffsetPosition(
        literalPos.source,
        offset
      ).asInstanceOf[c.universe.Position]
    }
    def compileTree(frag: WN.TemplateTree): Tree = {

//      println(frag)
      val fragPos = posFor(literalPos.point + frag.offset)

//      println(s"${frag.offset}\n${literalPos.point}\n${pos.point}\n$frag\n")

      val f: Tree = frag match {
        case WN.Plain(text, offset) => q"$text"
        case WN.Display(exp, offset) => compileTree(exp)
        case WN.Comment(msg, offset) => q""
        case WN.ScalaExp(Seq(WN.Simple(first, _), WN.Block(ws, args, content, _)), offset)
          if first.startsWith("for(") =>
          val fresh = c.fresh()
          val skeleton: Tree = c.parse(first + s"{$fresh}").asInstanceOf[Apply]
//          println("FIRST " + first)
          skeleton.foreach{x =>
            x
            if (x.pos != NoPosition) c.internal.setPos(x, posFor(x.pos.point + fragPos.point + 1))
          }
          val b = content.map(compileTree(_))
          def rec(t: Tree): Tree = t match {
            case a @ Apply(fun, List(f @ Function(vparams, body))) =>
              val f2 = Function(vparams, rec(body))
              val a2 = Apply(fun, List(f2))
              c.internal.setPos(a2, a.pos)
              c.internal.setPos(f2, f.pos)
              a2
            case Ident(x: TermName) if x.decoded == fresh =>
              q"Seq[$fragType](..$b)"
          }

          val out = rec(skeleton)


          out

        case WN.ScalaExp(WN.Simple(first, _) +: WN.Block(_, None, content1, _) +: rest, offset)
          if first.startsWith("if(") =>

          val b1 = content1.map(compileTree(_))
          val tree = c.parse(first + "{}").asInstanceOf[If]
          tree.foreach{x =>
            c.internal.setPos(x, posFor(x.pos.point + fragPos.point + 1))
          }
          val If(cond, _, _) = tree
          val b2 = rest match{
            case Seq(WN.Simple(next, _), WN.Block(_, None, content2, _)) =>
              content2.map(compileTree(_))
            case Seq() => Nil
          }
          q"if($cond){ Seq[$fragType](..$b1): $fragType } else { Seq[$fragType](..$b2): $fragType }"

        case xx @ WN.ScalaExp(WN.Simple(first, _) +: rest, offset) =>

          val firstTree = c.parse(first)

          firstTree.foreach{x =>
            c.internal.setPos(x, posFor(x.pos.point + fragPos.point))
          }

          val s = rest.foldLeft[Tree](firstTree) {
            case (l, WN.Simple(code, _)) =>
              val fresh = c.fresh()

              val snippet = s"$fresh$code"
              val skeleton = c.parse(snippet)

              def rec(t: Tree): Tree = {
                val newPos = posFor(fragPos.point + t.pos.point + first.length - fresh.length)
                val res = t match {
                  case Apply(fun, args) =>
                    for(arg <- args; tree <- arg if tree.pos != NoPosition){
                      c.internal.setPos(tree, posFor(tree.pos.point + newPos.point - t.pos.point))
                    }

                    Apply(rec(fun), args)
                  case Select(qualifier, name) => Select(rec(qualifier), name)
                  case Ident(x: TermName) if x.decoded == fresh => l
                }
                c.internal.setPos(res, newPos)

//                println(Position.formatMessage(newPos.asInstanceOf[scala.reflect.internal.util.Position], "", true))
                res
              }


              rec(skeleton)

            case (l, WN.Block(ws, None, content, _)) =>
              q"$l(..${content.map(compileTree(_))})"

            case (l, WN.Block(ws, Some(args), content, _)) =>
              val snippet = s"{$args ()}"
              val skeleton = c.parse(snippet)
              val Function(vparamss, body) = skeleton

              val func = Function(vparamss, q"Seq[$fragType](..${content.map(compileTree(_))})")
              c.internal.setPos(func, posFor(fragPos.point + skeleton.pos.point))
              q"$l($func)"
          }

          s
      }

//      f.foreach(_.pos = pos)

//      println("XXX " + f.pos)
//      println("F " + f)
      f
    }

    def compileTemplate(tmpl: WN.Template): Tree = {
      val WN.Template(name, comment, params, topImports, imports, subs, content, offset) = tmpl
      val fullName = if (name.toString == "") c.fresh("outer") else name

      val DefDef(mods, realName, tparams, vparamss, tpt, rhs) = {
        val snippet = s"def $fullName$params = {}"
        val z = c.parse(snippet).asInstanceOf[DefDef]
        z
      }

      val innerDefs = subs.map(compileTemplate(_))

      val body = atPos(literalPos)(q"""{
        import scalatags.Text.all._
        ..${topImports.map(i => c.parse(i.code))}
        ..$innerDefs

        Seq[scalatags.Text.all.Frag](..${content.map(compileTree(_))})
      }""")

      if (name.toString == "") body
      else DefDef(mods, realName, tparams, vparamss, tpt, body)
    }

    atPos(literalPos)(q"${compileTemplate(template)}")
  }
}