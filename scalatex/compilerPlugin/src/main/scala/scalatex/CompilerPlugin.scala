package scalatex

import java.nio.file.Paths

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualFile
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }

class CompilerPlugin(val global: Global) extends Plugin {
  import global._

  override def init(options: List[String],  error: String => Unit): Boolean = true

  val name = "scalatex"
  val description = "Compiles scalatex files into Scala compilation units"
  val components = List[PluginComponent](DemoComponent)
  private object DemoComponent extends PluginComponent {

    val global = CompilerPlugin.this.global
    import global._

    override val runsAfter = List("parser")
    override val runsBefore = List("namer")

    val phaseName = "Demo"

    override def newPhase(prev: Phase) = new GlobalPhase(prev) {
      val splitOptions = options.map(o => o.splitAt(o.indexOf(":")+1))
      val scalatexRoots = splitOptions.collect{case ("root:", p) => p}
      override def run() = {
        def recursiveListFiles(f: java.io.File): Iterator[java.io.File] = {
          val (dirs, files) =
            Option(f.listFiles())
              .toSeq
              .flatten
              .partition(_.isDirectory)
          files.iterator ++ dirs.iterator.flatMap(recursiveListFiles)
        }
        for {
          scalatexRoot <- scalatexRoots
          file <- recursiveListFiles(new java.io.File(scalatexRoot))
        } {
          val name = file.getCanonicalPath
          val fakeJfile = new java.io.File(name)
          val txt = io.Source.fromFile(name).mkString
          val virtualFile = new VirtualFile(name) {
            override def file = fakeJfile
          }
          val sourceFile = new BatchSourceFile(virtualFile, txt)
          val unit = new CompilationUnit(sourceFile)
          val objectName = name.slice(name.lastIndexOf('/')+1, name.lastIndexOf('.'))
          val pkgName =
            Paths.get(scalatexRoot)
                .relativize(fakeJfile.getParentFile.toPath)
                .toString
                .split("/")
                .map(s => s"package $s")
                .mkString("\n")

          val shim = s"""
            $pkgName
            import scalatags.Text.all._

            object $objectName{
              def apply() = scalatex.twf("${name}")
            }
          """
          unit.body = global.newUnitParser(shim).parse()
          global.currentRun.compileLate(unit)
        }
      }

      def name: String = phaseName

      def apply(unit: global.CompilationUnit): Unit = {}
    }
  }
}
