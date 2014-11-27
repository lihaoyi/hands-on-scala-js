
import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.merge.MergeStrategy
import org.eclipse.jgit.transport.{UsernamePasswordCredentialsProvider, RefSpec}

import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._

val cloneRepos = taskKey[Unit]("Clone stuff from github")

val sharedSettings = Seq(
  scalaVersion := "2.11.4",
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  autoCompilerPlugins := true
)

lazy val scalaParser = project.settings(sharedSettings:_*)
                              .settings(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "utest" % "0.2.4",
    "org.parboiled" %% "parboiled" % "2.0.1"
  ),
  testFrameworks += new TestFramework("utest.runner.JvmFramework")
)
lazy val scalatexApi = project.settings(sharedSettings:_*)
                              .dependsOn(scalaParser)
                              .settings(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "utest" % "0.2.4",
    "com.scalatags" %% "scalatags" % "0.4.2",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.parboiled" %% "parboiled" % "2.0.1"
  ),
  testFrameworks += new TestFramework("utest.runner.JvmFramework")
)

lazy val scalatexPlugin = Project(
  id   = "scalatexPlugin",
  base = file("scalatexPlugin"),
  dependencies = Seq(scalatexApi)
).settings(sharedSettings:_*).settings(
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  publishArtifact in Compile := false
)

lazy val book = Project(
  id = "book",
  base = file("book"),
  dependencies = Seq(scalatexApi)
).settings(sharedSettings:_*).settings(
  libraryDependencies ++= Seq(
    "org.webjars" % "highlightjs" % "8.2-1",
    "org.webjars" % "pure" % "0.5.0",
    "org.webjars" % "font-awesome" % "4.2.0",
    "com.lihaoyi" %%% "upickle" % "0.2.5"
  ),
  (resources in Compile) += {
    (fullOptJS in (demos, Compile)).value
    (artifactPath in (demos, Compile, fullOptJS)).value
  },
  (unmanagedResourceDirectories in Compile) ++=
    (unmanagedResourceDirectories in (demos, Compile)).value,
  scalacOptions in Compile ++= {
    val jar = (Keys.`package` in (scalatexPlugin, Compile)).value
    val addPlugin = "-Xplugin:" + jar.getAbsolutePath
    // add plugin timestamp to compiler options to trigger recompile of
    // main after editing the plugin. (Otherwise a 'clean' is needed.)
    val dummy = "-Jdummy=" + jar.lastModified
    val options = "-P:scalatex-options:" + sourceDirectory.value / "scalatex"
    Seq(addPlugin, dummy)
  },
  watchSources ++= {
    ((sourceDirectory in Compile).value / "scalatex" ** "*.scalatex").get
  },
  cloneRepos := {
    val localPath = target.value / "clones"
    if (!localPath.isDirectory){
      val paths = Seq(
        "scala-js" -> "scala-js",
        "lihaoyi" -> "workbench-example-app"
      )
      localPath.delete()
      for ((user, repo) <- paths){
        println(s"Cloning $repo...")
        Git.cloneRepository()
           .setURI(s"https://github.com/$user/$repo")
           .setDirectory(localPath / repo)
           .call()
      }
      println("Done Cloning")
    }else{
      println("Already Cloned")
    }
  },
  (run in Compile) <<= (run in Compile).dependsOn(cloneRepos),
  initialize := {
    System.setProperty("clone.root", target.value.getAbsolutePath + "/clones")
    System.setProperty("output.root", target.value.getAbsolutePath + "/output")
  },
  publish := {
    // If you want to publish the heroku app, push the contents of
    // examples/crossBuilds/clientserver to https://git.heroku.com/hands-on-scala-js.git
    // May aswell do it manually because it's a slow process and the
    // code doesn't change much

    val outputRoot = target.value.getAbsolutePath + "/output"
    val repo = Git.init().setDirectory(new File(outputRoot)).call()
    val remoteUrl = "https://github.com/lihaoyi/hands-on-scala-js"

    val creds = new UsernamePasswordCredentialsProvider(
      System.console.readLine("username>"),
      System.console.readPassword("password>")
    )
    repo.add()
        .addFilepattern(".")
        .call()

    repo.commit()
        .setAll(true)
        .setMessage(".")
        .call()

    repo.rebase().setUpstream("gh-pages").call()
    repo.fetch()
        .setRemote(remoteUrl)
        .setRefSpecs(new RefSpec("refs/heads/gh-pages:refs/heads/gh-pages"))
        .call()
    repo.push()
        .setRemote(remoteUrl)
        .setCredentialsProvider(creds)
        .setRefSpecs(new RefSpec("refs/heads/master:refs/heads/gh-pages"))
        .call()
    streams.value.log("Pushing to Github Pages complete!")
  }
)

lazy val demos = project.in(file("examples/demos"))

lazy val simple = project.in(file("examples/crossBuilds/simple"))

lazy val simple2 = project.in(file("examples/crossBuilds/simple2"))

lazy val clientserver = project.in(file("examples/crossBuilds/clientserver"))

lazy val client = ProjectRef(file("examples/crossBuilds/clientserver"), "client")

lazy val server = ProjectRef(file("examples/crossBuilds/clientserver"), "server")

lazy val clientserver2 = project.in(file("examples/crossBuilds/clientserver2"))

lazy val client2 = ProjectRef(file("examples/crossBuilds/clientserver2"), "client")

lazy val server2 = ProjectRef(file("examples/crossBuilds/clientserver2"), "server")

