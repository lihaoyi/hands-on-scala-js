(emitSourceMaps in fullOptJS) := false

enablePlugins(ScalaJSPlugin)

name := "Example"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided"

libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.2.6-RC1"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.7.0"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.4.3-RC1"

libraryDependencies += "com.lihaoyi" %%% "scalarx" % "0.2.7-RC1"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.2"