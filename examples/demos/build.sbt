Compile / fullOptJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) }

enablePlugins(ScalaJSPlugin)

name := "Example"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.11"

libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.9" % "provided"

libraryDependencies += "com.lihaoyi" %%% "upickle" % "1.1.0"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.1"

libraryDependencies += "com.lihaoyi" %%% "scalarx" % "0.4.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.7"
