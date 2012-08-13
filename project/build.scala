import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  override lazy val settings = super.settings ++
        Seq(resolvers := Seq(
          "sonatype-releases" at "https://oss.sonatype.org/content/repositories/snapshots/"
        ))

  val vault = Project(
    id = "vault"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "vault"
    , organization := "com.ephox"
    , version := "3.0-SNAPSHOT"
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.9" % "test" withSources
      , "org.scalatest" %% "scalatest" % "1.6.1" % "test" withSources
      , "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT"
      , "org.scalaz" %% "scalaz-iteratee" % "7.0-SNAPSHOT"
      , "org.hsqldb" % "hsqldb" % "2.0.0"
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
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.hsqldb" % "hsqldb" % "2.0.0"
      )
    )
  )
}
