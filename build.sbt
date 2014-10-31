import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._

lazy val api = project.in(file("api"))
                       .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.2.4",
      "com.scalatags" %% "scalatags" % "0.4.2",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
      compilerPlugin("org.scalamacros" % s"paradise" % "2.0.0" cross CrossVersion.full)
    ) ++ (
      if (scalaVersion.value startsWith "2.11.") Nil
      else Seq("org.scalamacros" %% s"quasiquotes" % "2.0.0")
    ),
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
    testFrameworks += new TestFramework("utest.runner.JvmFramework")
  )

lazy val book = Project(
  id = "book",
  base = file("book"),
  dependencies = Seq(api)
).settings(
  libraryDependencies ++= Seq(
    "org.webjars" % "highlightjs" % "8.2-1",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.eclipse.jgit" % "org.eclipse.jgit" % "3.5.1.201410131835-r"
  ),
  (resources in Compile) += {
    (fastOptJS in (examples, Compile)).value
    (artifactPath in (examples, Compile, fastOptJS)).value
  }
)
lazy val examples = project.in(file("examples"))