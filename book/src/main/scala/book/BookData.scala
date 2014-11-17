package book

import acyclic.file

object BookData {
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

