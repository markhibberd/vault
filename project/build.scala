import sbt._
import Keys._

object build extends Build {
  type Sett = Def.Setting[_]

  lazy val publishSetting =
    publishTo <<= version.apply(v => {
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      })

  val vault = Project(
    id = "vault"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "vault"
    , organization := "io.mth"
    , version := "0.1"
    , scalaVersion := "2.10.4"
    , publishSetting
    , publishMavenStyle := true
    , publishArtifact in Test := false
    , pomIncludeRepository := { _ => false }
    , licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause"))
    , homepage := Some(url("http://github.com/markhibberd/vault"))
    , pomExtra := (
      <scm>
        <url>git@github.com:markhibberd/vault.git</url>
        <connection>scm:git:git@github.com:markhibberd/vault.git</connection>
      </scm>
      <developers>
        <developer>
          <id>tonymorris</id>
          <name>Tony Morris</name>
          <url>http://tmorris.net</url>
        </developer>
        <developer>
          <id>mth</id>
          <name>Mark Hibberd</name>
          <url>http://mth.io</url>
        </developer>
      </developers>)
    , scalacOptions <++= scalaVersion map { v =>
        Seq("-deprecation", "-unchecked") ++ (if (v.contains("2.10"))
          Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps")
        else
          Seq())
      }
    , libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0.6"
      , "org.scalaz.stream" %% "scalaz-stream"% "0.4.1"
      , "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
      , "org.hsqldb" % "hsqldb" % "2.3.0"
      )
    )
  )

  val example = Project(
    id = "example"
  , base = file("example")
  , dependencies = Seq(vault)
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "example"
    , version := "1.0"
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.hsqldb" % "hsqldb" % "2.0.0"
      , "org.scalaz" %% "scalaz-core" % "7.0.6"
      )
    )
  )
}
