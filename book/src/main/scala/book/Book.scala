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
  val gettingStarted = twf("book/getting-started.tw")
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


  val txt = twf("book/index.tw").render

  object hli{
    def javascript(code: String*) = hl.highlight(code, "javascript", inline=true)
    def scala(code: String*) = hl.highlight(code, "scala", inline=true)
    def bash(code: String*) = hl.highlight(code, "bash", inline=true)
    def diff(code: String*) = hl.highlight(code, "diff", inline=true)
    def html(code: String*) = hl.highlight(code, "xml", inline=true)
  }
  object hl{
    def highlight(snippet: Seq[String], lang: String, inline: Boolean) = {
      val string = snippet.mkString
      val lines = string.split("\n", -1)
      if (inline){
        code(cls:=lang + " highlight-me", lines(0), padding:=0, display:="inline")
      }else{
        println("LINES " + lines.toList)
        println(snippet)
        val minIndent = lines.map(_.takeWhile(_ == ' ').length)
          .filter(_ > 0)
          .min
        val stripped = lines.map(_.drop(minIndent))
          .dropWhile(_ == "")
          .mkString("\n")

        pre(code(cls:=lang + " highlight-me", stripped))
      }
    }

    def javascript(code: String*) = highlight(code, "javascript", inline=false)
    def scala(code: String*) = highlight(code, "scala", inline=false)
    def bash(code: String*) = highlight(code, "bash", inline=false)
    def diff(code: String*) = highlight(code, "diff", inline=false)
    def html(code: String*) = highlight(code, "xml", inline=false)

    /**
     * Kinds of refs:
     *
     * Rule: Starting from a line, keep consuming until
     * the identation drops below the start
     *
     * def main = {
     *   /*example*/
     *   i am a cow
     *   hear me moo
     * }
     *
     * Rule: Starting from a line, keep consuming until
     * the indentation becomes equivalent to the current. If
     * it's a cosing brace, keep it.
     * val x = omg
     * val y = zzz
     *
     * class C{
     *
     * }
     */

    def ref(filepath: String, identifier: String = "", indented: Boolean = true) = {

      val lang = filepath.split('.').last match{
        case "js" => "javascript"
        case "scala" => "scala"
        case "sbt" => "scala"
        case "sh" => "bash"
        case "html" => "xml"
        case x =>
          println("??? " + x)
          ???
      }
      val lines = io.Source.fromFile(filepath).getLines().toVector
      val blob = if (identifier == ""){
        lines.mkString("\n")
      }else {
        val firstLine = lines.indexWhere(_.contains(identifier))
        val whitespace = lines(firstLine).indexWhere(!_.isWhitespace)
        val things =
          lines.drop(firstLine + 1)
            .takeWhile{ x =>
            val firstCharIndex = x.indexWhere(!_.isWhitespace)
            firstCharIndex == -1 || firstCharIndex >= whitespace + (if (indented) 1 else 0)
          }

        things.foreach(println)
        val stuff =
          if (!indented) {
            println("NOT INDENTED " + things)
            things
          }
          else {
            val last = lines(firstLine + things.length + 1)
//            println("LAST: " + last)
            if (last.trim.toSet subsetOf "}])".toSet) {
              lines(firstLine) +: things :+ last
            } else {
              lines(firstLine) +: things
            }
          }
        stuff.map(_.drop(whitespace)).mkString("\n")
      }
      println(blob)

      pre(code(cls:=lang + " highlight-me", blob))
    }
  }


}
