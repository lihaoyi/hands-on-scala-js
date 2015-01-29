val sharedSettings = Seq(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value  / "shared" / "main" / "scala",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "scalatags" % "0.4.3-RC1",
    "com.lihaoyi" %%% "upickle" % "0.2.6-RC1",
    "com.lihaoyi" %%% "autowire" % "0.2.4-RC1"
  ),
  scalaVersion := "2.11.5"
)

lazy val client = project.in(file("client"))
                         .enablePlugins(ScalaJSPlugin)
                         .settings(sharedSettings:_*)
                         .settings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.7.0"
  )
)

lazy val server = project.in(file("server"))
                         .settings(sharedSettings:_*)
                         .settings(
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.2",
    "io.spray" %% "spray-routing" % "1.3.2",
    "com.typesafe.akka" %% "akka-actor" % "2.3.6"
  ),
  (resources in Compile) += (fastOptJS in (client, Compile)).value.data
)

