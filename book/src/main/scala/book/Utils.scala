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
    val sbt = lnk("sbt", "https://www.scala-sbt.org/")
    val Mill = lnk("Mill", "https://www.lihaoyi.com/mill/")
    val IntelliJ = lnk("IntelliJ", "https://blog.jetbrains.com/scala/")
    val VSCode = lnk("VS Code", "https://scalameta.org/metals/docs/editors/vscode.html")
    val Nodejs = lnk("Node.js", "https://nodejs.org/")
    val jsdom = lnk("jsdom", "https://github.com/jsdom/jsdom")
    val jsdomJSEnv = lnk("scalajs-env-jsdom-nodejs", "https://github.com/scala-js/scala-js-env-jsdom-nodejs")
    val Selenium = lnk("Selenium", "https://www.selenium.dev/documentation/en/webdriver/")
    val SeleniumJSEnv = lnk("scalajs-env-selenium", "https://github.com/scala-js/scala-js-env-selenium")
    val PhantomJS = lnk("PhantomJS", "https://phantomjs.org/")
    val PhantomJSJSEnv = lnk("scalajs-env-phantomjs", "https://github.com/scala-js/scala-js-env-phantomjs")
    val Play = lnk("Play", "https://www.playframework.com/")
    val Scalatra = lnk("Scalatra", "http://www.scalatra.org/")
    val ScalaTest = lnk("ScalaTest", "http://www.scalatest.org/")
    val Scalate = lnk("Scalate", "https://github.com/scalate/scalate")
  }
  object github{
    val ScalaTags = lnk("ScalaTags", "https://github.com/lihaoyi/scalatags")
    val uPickle= lnk("uPickle", "https://github.com/lihaoyi/upickle")
    val scalaPickling = lnk("scala-pickling", "https://github.com/scala/pickling")
  }
}
