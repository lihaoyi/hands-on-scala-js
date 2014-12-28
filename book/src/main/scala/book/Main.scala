package book
import acyclic.file
import java.io.InputStream
import java.nio.file.{Paths, Files}

import scalatags.Text.{attrs, tags2, all}
import scalatags.Text.all._
import scalatex.site.Section.Tree
import scalatex.site.Site


object Main {
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

    val data = upickle.write(sect.structure)

    val s = new Site {
      def content = Map("index.html" -> Index())

      override def autoResources = super.autoResources | Set(
        "META-INF/resources/webjars/pure/0.5.0/grids-responsive-min.css",
        "css/side-menu.css",
        "example-opt.js",
        "webpage/weather.js",
        "favicon.svg",
        "favicon.png"
      )
      override def manualResources = super.manualResources | Set(
        "images/javascript-the-good-parts-the-definitive-guide.jpg",
        "images/Hello World.png",
        "images/Hello World White.png",
        "images/Hello World Console.png",
        "images/IntelliJ Hello.png",
        "images/Dropdown.png",
        "images/Scalatags Downloads.png"
      )
      override def headFrags = super.headFrags ++ Seq(
        meta(charset:="utf-8"),
        meta(name:="viewport", attrs.content:="width=device-width, initial-scale=1.0"),
        link(rel:="shortcut icon", `type`:="image/png", href:="favicon.png"),
        tags2.title("Hands-on Scala.js"),
        script(raw(googleAnalytics))
      )
      override def bodyFrag(frag: Frag) = body(
        onload:=s"Controller().main($data)",
        div(id:="layout")(
          a(href:="#menu", id:="menuLink", cls:="menu-link")(
            span
          ),
          div(id:="menu")

        ),
        div(id:="main",
          div(id:="main-box")(
            frag
          )
        )
      )

    }

    s.renderTo(System.getProperty("output.root") + "/")


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
