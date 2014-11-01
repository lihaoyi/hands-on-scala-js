import scala.reflect.internal.util.{BatchSourceFile, SourceFile, OffsetPosition}
import scala.reflect.io.{PlainFile, AbstractFile}
import scala.reflect.macros.{TypecheckException, Context}
import scalatags.Text.all._
import scalatex.stages.Compiler
import scala.language.experimental.macros
import acyclic.file

package object scalatex {
  import Util._
  /**
   * Wraps the given string as a twist fragment.
   */
  def tw(expr: String): Frag = macro Internals.applyMacro
  def twf(filename: String): Frag = macro Internals.applyMacroFile
  object Internals {

    def twRuntimeErrors(expr: String): Frag = macro applyMacroRuntimeErrors
    def twDebug(expr: String): Frag = macro applyMacroDebug

    def applyMacro(c: Context)(expr: c.Expr[String]): c.Expr[Frag] = applyMacroFull(c)(expr, false, false)
    def applyMacroDebug(c: Context)(expr: c.Expr[String]): c.Expr[Frag] = applyMacroFull(c)(expr, false, true)

    def applyMacroRuntimeErrors(c: Context)(expr: c.Expr[String]): c.Expr[Frag] = applyMacroFull(c)(expr, true, false)

    def applyMacroFile(c: Context)(filename: c.Expr[String]): c.Expr[Frag] = {
      import c.universe._
      val s = filename.tree
        .asInstanceOf[Literal]
        .value
        .value
        .asInstanceOf[String]
      val txt = io.Source.fromFile(s).mkString |> stages.IndentHandler
      val sourceFile = new BatchSourceFile(
        new PlainFile(s),
        txt.toCharArray
      )

      compileThing(c)(txt, sourceFile, 0, false, false)
    }

    case class DebugFailure(msg: String, pos: String) extends Exception(msg)

    private[this] def applyMacroFull(c: Context)
                      (expr: c.Expr[String],
                       runtimeErrors: Boolean,
                       debug: Boolean)
                      : c.Expr[Frag] = {
      import c.universe._
      val s = expr.tree
                  .asInstanceOf[Literal]
                  .value
                  .value
                  .asInstanceOf[String]
      val stringStart =
        expr.tree
          .pos
          .lineContent
          .drop(expr.tree.pos.column)
          .take(2)
      val indented = s |> stages.IndentHandler
      if (debug) println(indented)
      compileThing(c)(
        indented,
        expr.tree.pos.source,
        expr.tree.pos.point + (if (stringStart == "\"\"") 1 else -1),
        runtimeErrors,
        debug
      )
    }
  }

  def compileThing(c: Context)
                  (s: String,
                   source: SourceFile,
                   point: Int,
                   runtimeErrors: Boolean,
                   debug: Boolean) = {
    import c.universe._
    def compile(s: String): c.Tree = {
      val realPos = new OffsetPosition(source, point).asInstanceOf[c.universe.Position]

      Compiler(c)(realPos, s |> stages.Parser)
    }

    import c.Position
    try {
      val compiled = compile(s)
      if (debug) println(compiled)
      c.Expr[Frag](c.typeCheck(compiled))
    } catch {
      case e@TypecheckException(pos: Position, msg) =>
        if (!runtimeErrors) c.abort(pos, msg)
        else {
          val posMsg = pos.lineContent + "\n" + (" " * pos.column) + "^"
          c.Expr( q"""throw scalatex.Internals.DebugFailure($msg, $posMsg)""")
        }
    }

  }

}
