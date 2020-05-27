import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(Def.settings(
  scalaVersion := "2.12.11"
))

val library = crossProject(JSPlatform, JVMPlatform).settings(
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.2",
  testFrameworks += new TestFramework("utest.runner.Framework")
).jsSettings(
  // JS-specific settings here
).jvmSettings(
  // JVM-specific settings here
)

lazy val js = library.js

lazy val jvm = library.jvm
