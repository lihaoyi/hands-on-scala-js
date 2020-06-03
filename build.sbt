
import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.merge.MergeStrategy
import org.eclipse.jgit.transport.{UsernamePasswordCredentialsProvider, RefSpec}

val cloneRepos = taskKey[Unit]("Clone stuff from github")

inThisBuild(Def.settings(
  scalaVersion := "2.13.2",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-encoding",
    "utf-8",
    "-Xfatal-warnings",
  )
))

lazy val book = Project(
  id = "book",
  base = file("book")
).settings(scalatex.SbtPlugin.projectSettings:_*).settings(
  scalaVersion := "2.12.11",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "scalatex-site" % "0.3.11",
    "com.lihaoyi" %% "acyclic" % "0.1.9" % "provided",
  ),
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.9"),
  (resources in Compile) += (fullOptJS in (demos, Compile)).value.data,

  (unmanagedResourceDirectories in Compile) ++=
    (unmanagedResourceDirectories in (demos, Compile)).value,

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
  (managedClasspath in Runtime) := (managedClasspath in Runtime).dependsOn(cloneRepos).value,
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

    repo.push()
        .setRemote(remoteUrl)
        .setCredentialsProvider(creds)
        .setRefSpecs(new RefSpec("master:gh-pages"))
        .setForce(true)
        .call()

    streams.value.log("Pushing to Github Pages complete!")
  }
)

lazy val demos = project.in(file("examples/demos"))
