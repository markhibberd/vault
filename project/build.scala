import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  override lazy val settings = super.settings ++
        Seq(resolvers := Seq(
          "mth.io snapshots"  at "http://repo.mth.io/snapshots"
        , "mth.io releases"  at "http://repo.mth.io/releases"
        , "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
        , "releases"  at "http://oss.sonatype.org/content/repositories/releases"
        ))

  val vault = Project(
    id = "vault"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "vault"
    , organization := "com.ephox"
    , version := "4.0-SNAPSHOT"
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.9" % "test" withSources
      , "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT" withSources
      , "org.scalaz" %% "scalaz-iteratee" % "7.0-SNAPSHOT" withSources
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
      , "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT" withSources
      , "org.scalaz" %% "scalaz-iteratee" % "7.0-SNAPSHOT" withSources
      )
    )
  )
}
