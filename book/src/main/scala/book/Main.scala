package book
import acyclic.file
import java.io.InputStream
import java.nio.file.{Paths, Files}



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

    write(Book.site, "output/index.html")

    for(res <- Book.autoResources ++ Book.manualResources) {
      copy(getClass.getResourceAsStream("/" + res), "output/" + res)
    }
    sect.structure
    println("Writing Done")
  }


}
