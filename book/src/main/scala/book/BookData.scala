package book

import java.io.File

import acyclic.file
import ammonite.ops._
import ammonite.ops.Path
import scalatags.Text.TypedTag
import scalatags.Text.all._
import scalatex.site
import scalatex.site.Highlighter

object BookData {
  val wd = cwd
  val cloneRoot = Path(System.getProperty("clone.root"))
  val lnk = book.lnk
  val pureTable = book.pureTable
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

      val path = file.relativeTo(cloneRoot/root).toString.stripSuffix(".scala")
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

  lazy val hl = new Highlighter {
    override def pathMappings = Seq(
      cloneRoot/"scala-js" -> "https://github.com/scala-js/scala-js/blob/master",
      cloneRoot/"workbench-example-app" -> "https://github.com/lihaoyi/workbench-example-app/blob/master",
      wd -> "https://github.com/lihaoyi/hands-on-scala-js/blob/master"
    )
  }

  val sect = new site.Section{}
}
