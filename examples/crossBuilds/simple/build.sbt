val sharedSettings = Seq(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value  / "shared" / "main" / "scala"
)

lazy val js = project.in(file("js")).enablePlugins(ScalaJSPlugin)
                                    .settings(sharedSettings:_*)

lazy val jvm = project.in(file("jvm")).settings(sharedSettings:_*)