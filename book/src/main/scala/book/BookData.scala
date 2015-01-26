package book

import java.io.File

import acyclic.file
import ammonite.all._
import scalatags.Text.TypedTag
import scalatags.Text.all._
object BookData {
  val wd = processWorkingDir
  val cloneRoot = root/System.getProperty("clone.root").split('/')


  lazy val javaAPIs = {
    import java.io.File


    val roots = Seq(
      "scala-js"/'javalanglib/'src/'main/'scala,
      "scala-js"/'javalib/'src/'main/'scala
    )
    for{
      root <- roots
      file <- ls.rec! cloneRoot/root
      if file.ext == "scala"
    } yield{

      val path = (file - cloneRoot).toString.stripSuffix(".scala")
      val filename = path.replace('/', '.')

      val docpath = s"https://docs.oracle.com/javase/7/docs/api/$path.html"
      filename -> docpath
    }
  }
  var counter = 0
  def example(t: TypedTag[String], main: String) = {
    val tagId = "example"+counter
    counter += 1
    Seq(
      t(id:=tagId, display.block, overflow.scroll),
      script(s"$main(document.getElementById('$tagId'))")
    )
  }

  def split = div(cls:="pure-g")
  def more = div(cls:="pure-u-1 pure-u-md-13-24")
  def less = div(cls:="pure-u-1 pure-u-md-11-24")
  def half = div(cls:="pure-u-1 pure-u-md-1-2")


  val hl = new scalatex.site.Highlighter {
    override val pathMappings = Seq(
      cloneRoot/"scala-js" -> "https://github.com/scala-js/scala-js/blob/master",
      cloneRoot/"workbench-example-app" -> "https://github.com/lihaoyi/workbench-example-app/blob/master",
      wd -> "https://github.com/lihaoyi/hands-on-scala-js/blob/master"
    )
    override val suffixMappings = Map(
      "scala" -> "scala",
      "sbt" -> "scala",
      "js" -> "javascript"
    )
    def scala(s: String) = this.highlight(s, "scala")
    def bash(s: String) = this.highlight(s, "bash")
    def html(s: String) = this.highlight(s, "html")
    def xml(s: String) = this.highlight(s, "xml")
    def diff(s: String) = this.highlight(s, "diff")
    def javascript(s: String) = this.highlight(s, "javascript")
  }
}
