(emitSourceMaps in fullOptJS) := false

enablePlugins(ScalaJSPlugin)

name := "Example"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.5" % "provided"

libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.4"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.2"

libraryDependencies += "com.lihaoyi" %%% "scalarx" % "0.2.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.2"