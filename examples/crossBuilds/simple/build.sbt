import sbtcrossproject.CrossPlugin.autoImport.crossProject

val library = crossProject(JSPlatform, JVMPlatform).settings(
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.0",
  testFrameworks += new TestFramework("utest.runner.Framework")
).jsSettings(
  // JS-specific settings here
).jvmSettings(
  // JVM-specific settings here
)

lazy val js = library.js

lazy val jvm = library.jvm
