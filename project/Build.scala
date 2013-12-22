import sbt._
import Keys._

object ApplicationBuild extends Build {

  val appVersion      = "0.1"

  val dependencies    = Seq(
    "org.specs2" %% "specs2" % "2.3.6" // % "test"
  )

  val sless = Project(id = "sless", base = file("sless")).settings(
    libraryDependencies += "org.specs2" %% "specs2" % "2.3.6" // % "test"
  )

  val root = Project(id = "root", base = file("."))
    .aggregate(sless)

}


