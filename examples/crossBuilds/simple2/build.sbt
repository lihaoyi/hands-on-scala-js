import utest.jsrunner.JsCrossBuild

val cross = new JsCrossBuild(
  // Shared settings here
)

lazy val js = cross.js.settings(
  // JS-specific settings here
)

lazy val jvm = cross.jvm.settings(
  // JVM-specific settings here
)
