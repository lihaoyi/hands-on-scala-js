package book
import acyclic.file

import scalatags.Text.tags2
import scalatags.Text.all._

/**
 * Created by haoyi on 10/26/14.
 */
object Book {
  val autoResources = Set(
    "META-INF/resources/webjars/highlightjs/8.2-1/highlight.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/styles/idea.min.css",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/scala.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/javascript.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/bash.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/diff.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/xml.min.js",
    "META-INF/resources/webjars/pure/0.5.0/pure-min.css",
    "META-INF/resources/webjars/pure/0.5.0/grids-responsive-min.css",
    "META-INF/resources/webjars/font-awesome/4.2.0/fonts/FontAwesome.otf",
    "META-INF/resources/webjars/font-awesome/4.2.0/fonts/fontawesome-webfont.eot",
    "META-INF/resources/webjars/font-awesome/4.2.0/fonts/fontawesome-webfont.svg",
    "META-INF/resources/webjars/font-awesome/4.2.0/fonts/fontawesome-webfont.ttf",
    "META-INF/resources/webjars/font-awesome/4.2.0/fonts/fontawesome-webfont.woff",
    "css/side-menu.css",
    "example-opt.js",
    "example-opt.js.map",
    "webpage/weather.js",
    "favicon.svg",
    "favicon.png"
  )

  val fontAwesomeCss =
    "META-INF/resources/webjars/font-awesome/4.2.0/css/font-awesome.min.css"

  val manualResources = Set(
    "images/javascript-the-good-parts-the-definitive-guide.jpg",
    "images/Hello World.png",
    "images/Hello World White.png",
    "images/Hello World Console.png",
    "images/IntelliJ Hello.png",
    "images/Dropdown.png",
    "images/Scalatags Downloads.png",
    fontAwesomeCss
  )


  val txt = Index()
  val data = upickle.write(sect.structure)
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
  val site = Seq(
    raw("<!doctype html>"),
    html(
      head(
        meta(charset:="utf-8"),
        meta(name:="viewport", content:="width=device-width, initial-scale=1.0"),
        link(rel:="shortcut icon", `type`:="image/png", href:="favicon.png"),
        link(rel:="stylesheet", href:=fontAwesomeCss),
        link(rel:="stylesheet", href:="styles.css"),
        tags2.title("Hands-on Scala.js"),
        script(src:="scripts.js"),
        script(raw(googleAnalytics))
      ),
      body(
        div(id:="layout")(
          a(href:="#menu", id:="menuLink", cls:="menu-link")(
            span
          ),
          div(id:="menu")

        ),
        div(id:="main",
          div(id:="main-box")(
            txt
          )
        ),
        script(raw(s"Controller().main($data)"))
      )
    )
  ).render
}
