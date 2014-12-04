package scalatex

import sbt.Keys._
import sbt._
object SbtPlugin extends sbt.Plugin{
  val scalatexDirectory = taskKey[sbt.File]("Clone stuff from github")
  override val settings = Seq(
    scalatexDirectory := sourceDirectory.value / "scalatex",
    scalacOptions += {
      "-P:scalatex:root:" + scalatexDirectory.value.getCanonicalPath
    },
    watchSources += scalatexDirectory.value,
    addCompilerPlugin("com.lihaoyi" %% "scalatex-compiler-plugin" % "0.1.0"),
    libraryDependencies += "com.lihaoyi" %% "scalatex-api" % "0.1.0"
  )
}
