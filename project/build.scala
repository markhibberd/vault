import sbt._
import Keys._

object build extends Build {
  val buildServer = "http://jenkins/job"
  val scalaz7Version = "3"

  def scalaz7Core(scalaVersion: String) = "scalaz7_core" % "scalaz7_core" % scalaz7Version from "%s/Scalaz7/%s/artifact/core/target/scala-%s/scalaz-core_%s-7.0-SNAPSHOT.jar".format(buildServer, scalaz7Version, scalaVersion, scalaVersion)

  def scalaz7Iteratee(scalaVersion: String) = "scalaz7_iteratee" % "scalaz7_iteratee" % scalaz7Version from "%s/Scalaz7/%s/artifact/iteratee/target/scala-%s/scalaz-iteratee_%s-7.0-SNAPSHOT.jar".format(buildServer, scalaz7Version, scalaVersion, scalaVersion)

  def scalaz7Effect(scalaVersion: String) = "scalaz7_effect" % "scalaz7_effect" % scalaz7Version from "%s/Scalaz7/%s/artifact/effect/target/scala-%s/scalaz-effect_%s-7.0-SNAPSHOT.jar".format(buildServer, scalaz7Version, scalaVersion, scalaVersion)

  type Sett = Project.Setting[_]

  val vault = Project(
    id = "vault"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "vault"
    , organization := "com.ephox"
    , version := "1.0"
    , scalaVersion := "2.9.1"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test" withSources
      , "org.scalatest" %% "scalatest" % "1.6.1" % "test" withSources
      , "org.hsqldb" % "hsqldb" % "2.0.0"
      , scalaz7Core("2.9.1")
      , scalaz7Iteratee("2.9.1")
      , scalaz7Effect("2.9.1")
      )
    )
  )

  val example = Project(
    id = "example"
  , base = file("example")
  , dependencies = Seq(vault)
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "example"
    , organization := "com.ephox"
    , version := "1.0"
    , scalaVersion := "2.9.1"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
      // , "org.scalaz" %% "scalaz-core" % "6.0.4" withSources
        "org.hsqldb" % "hsqldb" % "2.0.0"
      )
    )
  )
}
