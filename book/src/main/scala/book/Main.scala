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

    val allNames = {
      def rec(n: Tree[String]): Seq[String] = {
        n.value +: n.children.flatMap(rec)
      }
      rec(sect.structure).toSet
    }
    val dupes = allNames.groupBy(x => x)
                        .values
                        .filter(_.size > 1)
                        .map(_.head)
                        .toSet

    assert(dupes.size == 0, s"Duplicate names: $dupes")

    val dangling = sect.usedRefs -- allNames

    assert(dangling.size == 0, s"Dangling Refs: $dangling")

    println("Writing Done")

    // can be used to verify that no links are broken
    // lnk.usedLinks
  }


}
