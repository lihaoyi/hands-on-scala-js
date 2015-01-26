package book
import acyclic.file
import java.io.InputStream
import java.nio.file.{Paths, Files}

import ammonite.ops.Path

import scalatags.Text.{attrs, tags2, all}
import scalatags.Text.all._
import scalatex.site.Section.Tree
import scalatex.site.Site
import ammonite.all.{rel => _, _}

object Main {
  val wd = processWorkingDir
  def main(args: Array[String]): Unit = {
    val googleAnalytics =
      """
        |(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
        |    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
        |  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
        |  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
        |
        |  ga('create', 'UA-27464920-4', 'auto');
        |  ga('send', 'pageview');
      """.stripMargin

    def data = upickle.write(sect.structure)

    val s = new Site {
      def content = Map("index.html" -> Index())

      override def autoResources = super.autoResources ++ Seq(
        root/"META-INF"/'resources/'webjars/'pure/"0.5.0"/"grids-responsive-min.css",
        root/'css/"side-menu.css",
        root/"example-opt.js",
        root/'webpage/"weather.js",
        root/"favicon.svg",
        root/"favicon.png"
      )

      override def manualResources = super.manualResources ++ Seq(
        root/'images/"javascript-the-good-parts-the-definitive-guide.jpg",
        root/'images/"Hello World.png",
        root/'images/"Hello World White.png",
        root/'images/"Hello World Console.png",
        root/'images/"IntelliJ Hello.png",
        root/'images/"Dropdown.png",
        root/'images/"Scalatags Downloads.png"
      )
      override def headFrags = super.headFrags ++ Seq(
        meta(charset:="utf-8"),
        meta(name:="viewport", attrs.content:="width=device-width, initial-scale=1.0"),
        link(rel:="shortcut icon", `type`:="image/png", href:="favicon.png"),
        tags2.title("Hands-on Scala.js"),
        script(raw(googleAnalytics))
      )
      override def bodyFrag(frag: Frag) = body(

        div(id:="layout")(
          a(href:="#menu", id:="menuLink", cls:="menu-link")(
            span
          ),
          div(id:="menu")

        ),
        div(
          id:="main",
          div(
            id:="main-box",
            cls:="scalatex-content",
            maxWidth:="840px",
            lineHeight:="1.6em",
            frag
          )
        ),
        onload:=s"Controller().main($data)"
      )
    }

    s.renderTo(Path(System.getProperty("output.root")))


    val allNames = {
      def rec(n: Tree[String]): Seq[String] = {
        n.value +: n.children.flatMap(rec)
      }
      rec(sect.structure).toSet
    }
    val dupes = allNames.groupBy(x => x)
                        .values
                        .filter(_.size > 1)
                        .map(_.head)
                        .toSet

    assert(dupes.size == 0, s"Duplicate names: $dupes")

    val dangling = sect.usedRefs -- allNames

    assert(dangling.size == 0, s"Dangling Refs: $dangling")

    println("Writing Done")

    // can be used to verify that no links are broken
    // lnk.usedLinks
  }

}
