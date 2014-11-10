import utest.jsrunner.JsCrossBuild
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._

val cross = new JsCrossBuild(
  // Shared settings here
)

lazy val js = cross.js.settings(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6"
  )
)

lazy val jvm = cross.jvm.settings(
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.2",
    "io.spray" %% "spray-routing" % "1.3.2",
    "com.typesafe.akka" %% "akka-actor" % "2.3.6"
  ),
  (resources in Compile) += {
    (fastOptJS in (js, Compile)).value
    (artifactPath in (js, Compile, fastOptJS)).value
  }
)
