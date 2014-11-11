package book
import acyclic.file
import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}

import org.eclipse.jgit.api.Git

import scala.collection.mutable
import scalatags.Text.all._
import scalatags.Text.tags2


object Main {
  def write(txt: String, dest: String) = {
    Paths.get(dest).toFile.getParentFile.mkdirs()
    Files.deleteIfExists(Paths.get(dest))
    Files.write(Paths.get(dest), txt.getBytes)
  }
  def copy(src: InputStream, dest: String) = {
    Paths.get(dest).toFile.getParentFile.mkdirs()
    Files.deleteIfExists(Paths.get(dest))
    Files.copy(src, Paths.get(dest))
  }

  val txt = Index.template
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

  def main(args: Array[String]): Unit = {
    println("Writing Book")


    write(site, "output/index.html")

    for(res <- Utils.autoResources ++ Utils.manualResources) {
      copy(getClass.getResourceAsStream("/" + res), "output/" + res)
    }
    println("Writing Done")
  }


}
