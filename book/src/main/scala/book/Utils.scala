package book

import acyclic.file
import scala.collection.mutable
import scalatags.Text.all._
import scalatags.text.Builder

case class pureTable(header: Frag*){
  def apply(content: Frag*) = {
    table(cls:="pure-table pure-table-horizontal half-table")(
      thead(header),
      tbody(content)
    )
  }
}
object sect extends scalatex.site.Section{
  var indent = 0

  override val headers: Seq[Header] = Seq(
    Header(
      (l, h, s) => div(cls:="header")(h1(h, l), br, h2(s)),
      f => div(cls:="content", f)
    ),
    Header(
      (l, h, s) => div(cls:="header")(h1(id:=munge(h), h, l), br)),
    h1, h2, h3, h4, h5, h6
  )
}


object lnk{
  val usedLinks = mutable.Set.empty[String]
  def apply(name: String, url: String) = {
    usedLinks.add(url)
    a(name, href:=url)
  }
  object dom{
    def mdnThing(name: String, scalaJsName: String = null) = lnk(
      Option(scalaJsName).getOrElse(name),
      "https://developer.mozilla.org/en-US/docs/Web/API/" + name
    )
    def mdnEvent(name: String) = lnk(name, "https://developer.mozilla.org/en-US/docs/Web/Events/" + name)
    val CanvasRenderingContext2D = mdnThing("CanvasRenderingContext2D")
    val Element = mdnThing("Element")
    object html{
      val Canvas = mdnThing("HTMLCanvasElement", "html.Canvas")
      val Element = mdnThing("HTMLElement", "html.Element")
      val Input = mdnThing("HTMLInputElement", "html.Input")
      val Span = mdnThing("HTMLSpanElement", "html.Span")
    }
    val XMLHttpRequest = mdnThing("XMLHttpRequest")
    val getElementById = mdnThing("document.getElementById")
    val setInterval = mdnThing("WindowTimers.setInterval")
    val mousedown = mdnEvent("mousedown")
    val mouseup = mdnEvent("mouseup")
    val onclick = mdnEvent("onclick")
    val onkeyup = mdnEvent("onkeyup")
    val JSONparse = lnk("Json.parse", "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse")
  }
  object scala{
    def apply(s: String) = {
      lnk(s, "http://www.scala-lang.org/files/archive/nightly/docs/library/index.html#" + s)
    }
  }
  object misc{
    val IntelliJ = lnk("IntelliJ", "http://blog.jetbrains.com/scala/")
    val Eclipse = lnk("Eclipse", "http://scala-ide.org/")
    val Rhino = lnk("Rhino", "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino")
    val Nodejs = lnk("Node.js", "http://nodejs.org/")
    val PhantomJS = lnk("PhantomJS", "http://phantomjs.org/")
    val Play = lnk("Play", "https://www.playframework.com/")
    val Scalatra = lnk("Scalatra", "http://www.scalatra.org/")
    val ScalaTest = lnk("ScalaTest", "http://www.scalatest.org/")
    val Scalate = lnk("Scalate", "https://github.com/scalate/scalate")
  }
  object github{
    val Scalatags = lnk("Scalatags", "https://github.com/lihaoyi/scalatags")
    val uPickle= lnk("uPickle", "https://github.com/lihaoyi/upickle")
    val scalaPickling = lnk("scala-pickling", "https://github.com/scala/pickling")
  }
}
