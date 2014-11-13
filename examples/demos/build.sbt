import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys.jsDependencies

scalaJSSettings

name := "Example"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "0.5.1"

jsDependencies += "org.webjars" % "react" % "0.11.1" / "react-with-addons.js" commonJSName "React"

libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.2.5"

libraryDependencies += "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6"

libraryDependencies += "com.scalatags" %%% "scalatags" % "0.4.2"