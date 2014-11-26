package book

import java.io.File

import acyclic.file
import scalatags.Text.TypedTag
import scalatags.Text.all._
object BookData {
  val cloneRoot = System.getProperty("clone.root") + "/"


  lazy val javaAPIs = {
    import java.io.File
    def recursiveListFiles(f: File): Array[File] = {
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }

    val roots = Seq(
      "scala-js/javalanglib/src/main/scala",
      "scala-js/javalib/src/main/scala"
    )
    for{
      root <- roots
      file <- recursiveListFiles(new File(cloneRoot + root))
      if file != null
      if file.isFile
    } yield{
      val path = file.getPath
        .drop(cloneRoot.length + root.length + 1)
        .dropRight(".scala".length)
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
      t(id:=tagId, display.block),
      script(s"$main(document.getElementById('$tagId'))")
    )
  }
  def split = div(cls:="pure-g")
  def more = div(cls:="pure-u-1 pure-u-md-13-24")
  def less = div(cls:="pure-u-1 pure-u-md-11-24")
  def half = div(cls:="pure-u-1 pure-u-md-1-2")


  val hl = new Highlighter(
    Seq(
      s"$cloneRoot/scala-js" -> "https://github.com/scala-js/scala-js",
      s"$cloneRoot/workbench-example-app" -> "https://github.com/lihaoyi/workbench-example-app",
      "" -> "https://github.com/lihaoyi/hands-on-scala-js"
    )
  )
}
