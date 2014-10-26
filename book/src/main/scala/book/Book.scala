package book

import twist._

import scalatags.Text.tags2
import scala.collection.mutable
import scalatags.Text.all._

/**
 * Created by haoyi on 10/26/14.
 */
object Book {

  import Utils.sect

  val intro = twf("book/intro.tw")
  val contentBar = {
    def rec(current: Node, depth: Int): Frag = {
      div(
        marginLeft := s"${depth * 5}px",
        a(current.name, href:="#"+Utils.munge(current.name)),
        current.children.map(
          rec(_, depth + 1)
        )
      )
    }
    //    @li(cls:="menu-item-divided pure-menu-selected")
    ul(rec(Utils.structure, 0))
  }
  println(contentBar)

  val txt = twf("book/index.tw").render

  object highlight{
    def highlight(snippet: Seq[String], lang: String) = {
      pre(code(cls:=lang, snippet.mkString))
    }

    def javascript(code: String*) = highlight(code, "javascript")
    def scala(code: String*) = highlight(code, "scala")
    def bash(code: String*) = highlight(code, "bash")
  }
}
