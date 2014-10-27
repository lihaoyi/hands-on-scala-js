package book

import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}

import org.eclipse.jgit.api.Git

import scala.collection.mutable
import scalatags.Text.all._
import scalatags.Text.tags2


object Main {
  def write(txt: String, dest: String) = {
    Paths.get(dest).toFile.getParentFile.mkdirs()
    Files.deleteIfExists(Paths.get(dest))
    Files.write(Paths.get(dest), txt.getBytes)
  }
  def copy(src: InputStream, dest: String) = {
    Paths.get(dest).toFile.getParentFile.mkdirs()
    Files.deleteIfExists(Paths.get(dest))
    Files.copy(src, Paths.get(dest))
  }

  def main(args: Array[String]): Unit = {
    println("Writing Book")
//    Files.deleteIfExists(Paths.get("output/temp"))
//    Git.cloneRepository()
//       .setURI("https://github.com/lihaoyi/workbench-example-app")
//       .setDirectory(new java.io.File("output/temp"))
//       .call()


    write(Book.txt, "output/index.html")

    for(res <- Utils.autoResources ++ Utils.manualResources) {
      copy(getClass.getResourceAsStream("/" + res), "output/" + res)
    }
    println("Writing Done")
  }


}
