import NativePackagerKeys._
import utest.jsrunner.JsCrossBuild
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
val sharedSettings = Seq(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value  / "shared" / "main" / "scala",
  libraryDependencies ++= Seq(
    "com.scalatags" %%% "scalatags" % "0.4.2",
    "com.lihaoyi" %%% "upickle" % "0.2.5"
  ),
  scalaVersion := "2.11.4"
)

lazy val client = project.in(file("client"))
                         .settings(scalaJSSettings:_*)
                         .settings(sharedSettings:_*)
                         .settings(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6"
  )
)

lazy val server = project.in(file("server"))
                         .settings(sharedSettings:_*)
                         .settings(packageArchetype.java_application:_*)
                         .settings(
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.2",
    "io.spray" %% "spray-routing" % "1.3.2",
    "com.typesafe.akka" %% "akka-actor" % "2.3.6"
  ),
  (resources in Compile) += {
    (fastOptJS in (client, Compile)).value
    (artifactPath in (client, Compile, fastOptJS)).value
  }
)

