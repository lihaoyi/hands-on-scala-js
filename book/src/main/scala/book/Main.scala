package book
import acyclic.file
import java.io.InputStream
import java.nio.file.{Paths, Files}

import ammonite.ops.Path

import scalatags.Text.{attrs, tags2, all}
import scalatags.Text.all._
import scalatex.site

import scalatex.site.{Tree, Site}
import ammonite.ops._

object Main {
  val wd = pwd

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



    val s = new Site {
      def content: Map[String, Page] =
        Map("index.html" -> ((defaultHeader, scalatex.Index())))

      override def autoResources: Seq[ResourcePath] =
        super.autoResources ++
        BookData.hl.autoResources ++
        site.Sidebar.autoResources ++ Seq[ResourcePath](
        webjars/'pure/"0.6.0"/"grids-responsive-min.css",
        resource/'css/"side-menu.css",
        resource/"example-opt.js",
        resource/'webpage/"weather.js",
        resource/"favicon.svg",
        resource/"favicon.png"
      )

      override def manualResources: Seq[ResourcePath] =
        super.manualResources ++ Seq(
        resource/'images/"javascript-the-good-parts-the-definitive-guide.jpg",
        resource/'images/"Hello World.png",
        resource/'images/"Hello World White.png",
        resource/'images/"Hello World Console.png",
        resource/'images/"IntelliJ Hello.png",
        resource/'images/"Dropdown.png",
        resource/'images/"Scalatags Downloads.png"
      )
      override def defaultHeader: Seq[Frag] = super.defaultHeader ++ Seq(
        meta(charset:="utf-8"),
        meta(name:="viewport", attrs.content:="width=device-width, initial-scale=1.0"),
        link(rel:="shortcut icon", `type`:="image/png", href:="favicon.png"),
        tags2.title("Hands-on Scala.js"),
        script(raw(googleAnalytics))
      )
      override def bodyFrag(frag: Frag): Frag = body(
        super.bodyFrag(frag),
        site.Sidebar.snippet(BookData.sect.structure.children),
        scalatex.site.Highlighter.snippet
      )
    }

    customRenderToWorkaroundSiteIssue(s, Path(System.getProperty("output.root")))

    val allNames = {
      def rec(n: Tree[String]): Seq[String] = {
        n.value +: n.children.flatMap(rec)
      }
      rec(BookData.sect.structure).toSet
    }
    val dupes = allNames.groupBy(x => x)
                        .values
                        .filter(_.size > 1)
                        .map(_.head)
                        .toSet

    assert(dupes.size == 0, s"Duplicate names: $dupes")

    val dangling = BookData.sect.usedRefs -- allNames

    assert(dangling.size == 0, s"Dangling Refs: $dangling")

    println("Writing Done")

    // can be used to verify that no links are broken
    // lnk.usedLinks
  }

  // Work around https://github.com/lihaoyi/Scalatex/issues/72
  private def customRenderToWorkaroundSiteIssue(s: Site, outputRoot: Path): Unit = {
    def generateHtml(outputRoot: Path): Unit = {
      for ((path, (pageHeaders, pageBody)) <- s.content){
        val txt = html(
          head(pageHeaders),
          body(s.bodyFrag(pageBody))
        ).render
        val cb = java.nio.CharBuffer.wrap("<!DOCTYPE html>" + txt)
        val bytes = scala.io.Codec.UTF8.encoder.encode(cb)
        write.over(outputRoot/path,
          bytes.array().slice(bytes.position(), bytes.limit()))
      }
    }

    generateHtml(outputRoot)
    s.bundleResources(outputRoot)
  }

}
