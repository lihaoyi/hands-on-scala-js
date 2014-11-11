package book
import acyclic.file

import scalatags.Text.tags2
import scalatags.Text.all._

/**
 * Created by haoyi on 10/26/14.
 */
object Book {
  val autoResources = Seq(
    "META-INF/resources/webjars/highlightjs/8.2-1/highlight.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/styles/idea.min.css",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/scala.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/javascript.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/bash.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/diff.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/xml.min.js",
    "css/pure-min.css",
    "css/grids-responsive-min.css",
    "css/layouts/side-menu.css",
    "js/ui.js",
    "example-fastopt.js",
    "webpage/weather.js",
    "favicon.svg",
    "favicon.png"
  )

  val manualResources = Seq(
    "images/javascript-the-good-parts-the-definitive-guide.jpg",
    "images/Hello World.png",
    "images/Hello World White.png",
    "images/Hello World Console.png",
    "images/IntelliJ Hello.png",
    "images/Dropdown.png",
    "images/Scalatags Downloads.png"
  )

  val includes = for(res <- autoResources) yield {
    if (res.endsWith(".js"))
      script(src:=res)
    else if (res.endsWith(".css"))
      link(rel:="stylesheet", href:=res)
    else
      raw("")
  }

  val txt = Index()
  val contentBar = {
    def rec(current: Node, depth: Int): Seq[Frag] = {
      println("\t"*depth + current.name)
      Seq(
        li(
          a(
            current.name,
            href:="#"+sect.munge(current.name),
            paddingLeft := s"${depth * 10 + 10}px",
            cls := "menu-item" + (if (depth == 1) " menu-item-divided " else "")
          )
        )
      ) ++ current.children.flatMap(rec(_, depth + 1))
    }

    println("TABLE OF CONTENTS")
    rec(sect.structure, 0)
  }
  val site = Seq(
    raw("<!doctype html>"),
    html(
      head(
        meta(charset:="utf-8"),
        meta(name:="viewport", content:="width=device-width, initial-scale=1.0"),
        link(rel:="shortcut icon", `type`:="image/png", href:="favicon.png"),
        tags2.title("Hands-on Scala.js"),
        includes
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



}
