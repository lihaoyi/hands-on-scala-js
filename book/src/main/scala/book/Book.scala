package book
import acyclic.file
import scalatex._

import scalatags.Text.tags2
import scala.collection.mutable
import scalatags.Text.all._

/**
 * Created by haoyi on 10/26/14.
 */
object Book {
  import Utils.sect
  val myTable = Seq(
    ("Most of java.lang.*", "j.l.Thread, j.l.Runtime, ..."),
    ("Almost all of scala.*", "s.c.parallel, s.tools.nsc"),
    ("Some of java.util.*", "org.omg.CORBA, sun.misc.*"),
    ("Scala Macros: upickle, scala-async, scalaxy, etc", "Reflection: scala-pickling, scala-reflect"),
    ("Pure-Scala ecosystem: shapeless, scalaz, scalatags, utest", "Java-dependent: Scalatest, Scalate"),
    ("JS stuff: XmlHttpRequest, Websockets. Localstorage", " JVM stuff: Netty, akka, spray, file IO, JNI"),
    ("HTML DOM, Canvas, WebGL", "AWT, Swing, SWT, OpenGL"),
    ("JavaScript libraries: chipmunk.js, hand.js, react.js, jquery", "Java ecosystem: guice, junit, apache-commons, log4j"),
    ("IntelliJ, Eclipse, SBT, Chrome console, firebug", "Scala REPL, Yourkit, VisualVM, JProfiler")
  )


  object hl{
    def highlight(snippet: Seq[String], lang: String) = {
      val string = snippet.mkString
      val lines = string.split("\n", -1)
      if (lines.length == 1){
        code(cls:=lang + " highlight-me", lines(0), padding:=0, display:="inline")
      }else{
        val minIndent = lines.map(_.takeWhile(_ == ' ').length)
          .filter(_ > 0)
          .min
        val stripped = lines.map(_.drop(minIndent))
          .dropWhile(_ == "")
          .mkString("\n")

        pre(code(cls:=lang + " highlight-me", stripped))
      }
    }

    def javascript(code: String*) = highlight(code, "javascript")
    def scala(code: String*) = highlight(code, "scala")
    def bash(code: String*) = highlight(code, "bash")
    def diff(code: String*) = highlight(code, "diff")
    def html(code: String*) = highlight(code, "xml")

    def ref(filepath: String, start: String = "", end: String = "\n") = {

      val lang = filepath.split('.').last match {
        case "js" => "javascript"
        case "scala" => "scala"
        case "sbt" => "scala"
        case "sh" => "bash"
        case "html" => "xml"
        case x =>
          println("??? " + x)
          ???
      }

      val lines = io.Source.fromFile(filepath).getLines().toVector

      def indent(line: String) = line.takeWhile(_.isWhitespace).length

      val startLine = lines.indexWhere(_.contains(start))
      if (startLine == -1){
        throw new Exception("Can't find marker: " + start)
      }
      val whitespace = indent(lines(startLine))
      val endLine = lines.indexWhere(
        line => line.contains(end) || (indent(line) < whitespace && line.trim != ""),
        startLine
      )
      val sliced =
        if (endLine == -1) lines.drop(startLine)
        else lines.slice(startLine, endLine)
      val blob = sliced.map(_.drop(whitespace)).mkString("\n")


      pre(code(cls:=lang + " highlight-me", blob))
    }
  }
  import java.io.File
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  lazy val javaAPIs = {
    val roots = Seq(
      "output/scala-js/javalanglib/src/main/scala",
      "output/scala-js/javalib/src/main/scala"
    )
    for{
      root <- roots
      file <- recursiveListFiles(new File(root))
      if file != null
      if file.isFile
    } yield{
      val path = file.getPath
                     .drop(root.length + 1)
                     .dropRight(".scala".length)
      val filename = path.replace('/', '.')
      val docpath = s"https://docs.oracle.com/javase/7/docs/api/$path.html"
      filename -> docpath
    }
  }
}
