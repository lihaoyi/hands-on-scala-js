
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._


lazy val scalatexApi = project.in(file("scalatexApi"))
                       .settings(
  scalaVersion := "2.11.4",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "utest" % "0.2.4",
    "com.scalatags" %% "scalatags" % "0.4.2",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
    "org.parboiled" %% "parboiled" % "2.0.1"
  ),
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  testFrameworks += new TestFramework("utest.runner.JvmFramework")
)

lazy val scalatexPlugin = Project(
  id   = "scalatexPlugin",
  base = file("scalatexPlugin"),
  dependencies = Seq(scalatexApi)
) settings (
  scalaVersion := "2.11.4",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  publishArtifact in Compile := false
)

lazy val book = Project(
  id = "book",
  base = file("book"),
  dependencies = Seq(scalatexApi)
).settings(
  scalaVersion := "2.11.4",
  libraryDependencies ++= Seq(
    "org.webjars" % "highlightjs" % "8.2-1",
    "org.webjars" % "pure" % "0.5.0",
    "org.webjars" % "font-awesome" % "4.2.0",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.eclipse.jgit" % "org.eclipse.jgit" % "3.5.1.201410131835-r",
    "com.lihaoyi" %%% "upickle" % "0.2.5"
  ),
  (resources in Compile) += {
    (fastOptJS in (demos, Compile)).value
    (artifactPath in (demos, Compile, fastOptJS)).value
  },
  (unmanagedResourceDirectories in Compile) ++=
    (unmanagedResourceDirectories in (demos, Compile)).value,
  scalacOptions in Compile ++= {
    val jar = (Keys.`package` in (scalatexPlugin, Compile)).value
    val addPlugin = "-Xplugin:" + jar.getAbsolutePath
    // add plugin timestamp to compiler options to trigger recompile of
    // main after editing the plugin. (Otherwise a 'clean' is needed.)
    val dummy = "-Jdummy=" + jar.lastModified
    val options = "-P:scalatex-options:" + sourceDirectory.value / "scalatex"
    Seq(addPlugin, dummy)
  },
  watchSources ++= {
    ((sourceDirectory in Compile).value / "scalatex" ** "*.scalatex").get
  },
  (watchSources in Test) ++= {
    ((sourceDirectory in Test).value / "scalatex" ** "*.scalatex").get
  },
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2")
)

lazy val demos = project.in(file("examples/demos"))

lazy val simple = project.in(file("examples/crossBuilds/simple"))

lazy val simple2 = project.in(file("examples/crossBuilds/simple2"))

lazy val client = ProjectRef(file("examples/crossBuilds/clientserver"), "client")

lazy val server = ProjectRef(file("examples/crossBuilds/clientserver"), "server")

