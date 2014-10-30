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

  lazy val intro = sect("Intro to Scala.js")(twf("book/intro.tw"))
  lazy val gettingStarted = sect("Getting Started")(twf("book/getting-started.tw"))
  lazy val canvasApp = sect("Canvas App")(twf("book/canvas-app.tw"))
  val txt = twf("book/index.tw")
  val contentBar = {
    def rec(current: Node, depth: Int): Seq[Frag] = {
      println("\t"*depth + current.name)
      Seq(
        li(
          a(
            current.name,
            href:="#"+Utils.munge(current.name),
            paddingLeft := s"${depth * 10 + 10}px",
            cls := "menu-item" + (if (depth == 1) " menu-item-divided " else "")
          )
        )
      ) ++ current.children.flatMap(rec(_, depth + 1))
    }

    println("TABLE OF CONTENTS")
    rec(Utils.structure, 0)
  }
  val site = Seq(
    raw("<!doctype html>"),
    html(
      head(
        meta(charset:="utf-8"),
        meta(name:="viewport", content:="width=device-width, initial-scale=1.0"),
        tags2.title("Hands-on Scala.js"),
        Utils.includes
      ),

      div(id:="layout")(
        a(href:="#menu", id:="menuLink", cls:="menu-link")(
          span
        ),

        div(id:="menu")(
          div(cls:="pure-menu pure-menu-open")(
            a(cls:="pure-menu-heading", href:="#")(
              "Contents"
            ),
            ul(cls:="menu-item-list")(
              contentBar
            )
          )
        )
      ),
      div(id:="main",
        div(id:="main-box")(
          txt
        )
      )
    )
  ).render

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

    def ref(filepath: String, identifier: String = "", end: String = "", indented: Boolean = true) = {

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
                  if (end == "") {
                    val firstCharIndex = x.indexWhere(!_.isWhitespace)
                    firstCharIndex == -1 || firstCharIndex >= whitespace + (if (indented) 1 else 0)
                  }else{
                    !x.contains(end)
                  }
               }

        val stuff =
          if (!indented) {
            things
          } else {
            val last = lines(firstLine + things.length + 1)
            if (last.trim.toSet subsetOf "}])".toSet) {
              lines(firstLine) +: things :+ last
            } else {
              lines(firstLine) +: things
            }
          }
        stuff.map(_.drop(whitespace)).mkString("\n")
      }
      pre(code(cls:=lang + " highlight-me", blob))
    }
  }
}
