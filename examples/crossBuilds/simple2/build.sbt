
val cross = crossProject.settings(
  // Shared settings here
).jsSettings(
  // JS-specific settings here
).jvmSettings(
  // JVM-specific settings here
)

lazy val js = cross.js

lazy val jvm = cross.jvm
