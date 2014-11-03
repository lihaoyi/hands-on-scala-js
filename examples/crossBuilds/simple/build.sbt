lazy val js = project.in(file("js")).settings(scalaJSSettings:_*).settings(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value / "shared" / "main" / "scala"
)
lazy val jvm = project.in(file("jvm")).settings(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value  / "shared" / "main" / "scala"
)