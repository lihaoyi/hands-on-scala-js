import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
val sharedSettings = Seq(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value  / "shared" / "main" / "scala",
  libraryDependencies ++= Seq(
    "com.scalatags" %%% "scalatags" % "0.4.2",
    "com.lihaoyi" %%% "upickle" % "0.2.5",
    "com.lihaoyi" %%% "autowire" % "0.2.3"
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
                         .settings(
  libraryDependencies ++= {
    val akkaV = "2.3.5"
    val sprayV = "1.3.2"
    Seq(
      "io.spray"            %%  "spray-servlet" % sprayV,
      "io.spray"            %%  "spray-routing" % sprayV,
      "io.spray"            %%  "spray-testkit" % sprayV % "test",
      "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
      "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
      "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test"
    )
  },
  (resources in Compile) += {
    (fastOptJS in (client, Compile)).value
    (artifactPath in (client, Compile, fastOptJS)).value
  }
)

