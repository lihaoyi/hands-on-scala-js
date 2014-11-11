package book

import acyclic.file

object BookData {
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

  lazy val javaAPIs = {
    import java.io.File
    def recursiveListFiles(f: File): Array[File] = {
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }

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

