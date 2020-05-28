/*build.sbt*/

inThisBuild(Def.settings(
  scalaVersion := "2.12.11"
))

val app = crossProject(JSPlatform, JVMPlatform).settings(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value  / "shared" / "main" / "scala",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "scalatags" % "0.9.1",
    "com.lihaoyi" %%% "upickle" % "1.1.0"
  )
).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "1.0.0"
  )
).jvmSettings(
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.6.5",
    "com.typesafe.akka" %% "akka-stream" % "2.6.5",
    "com.typesafe.akka" %% "akka-http" % "10.1.12",
    "org.webjars" % "bootstrap" % "3.2.0"
  )
)

lazy val appJS = app.js
lazy val appJVM = app.jvm.settings(
  (resources in Compile) += (fastOptJS in (appJS, Compile)).value.data
)
