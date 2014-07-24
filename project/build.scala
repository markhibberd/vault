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
    , version := "0.5.1"
    , scalaVersion := "2.10.4"
    , scalacOptions ++= Seq("-target:jvm-1.6", "-deprecation", "-unchecked", "-feature", "-language:_", "-Ywarn-all", "-Xlint", "-Xfatal-warnings", "-Yinline-warnings")
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
        "org.scalaz"        %% "scalaz-core"         % "7.0.6"
      , "org.scalaz.stream" %% "scalaz-stream"       % "0.4.1"
      , "com.chuusai"       %% "shapeless"           % "2.0.0"            cross CrossVersion.full
      , "org.scalacheck"    %% "scalacheck"          % "1.10.1"  % "test"
      , "org.specs2"        %% "specs2"              % "2.3.11"  % "test"
      , "org.hsqldb"        %  "hsqldb"              % "2.3.0"   % "test"
      )
    , resolvers ++= Seq(
        Resolver.sonatypeRepo("releases")
      , Resolver.typesafeRepo("releases")
      , "Scalaz Bintray Repo"   at "http://dl.bintray.com/scalaz/releases"
      )
    )
  )

  lazy val benchmark = Project(
    id = "benchmark"
  , base = file("benchmark")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "vault-benchmark"
    , scalaVersion := "2.10.4"
    , fork in run := true
    , javaOptions in run <++= (fullClasspath in Runtime).map(cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")))
    ) ++ Seq[Sett](libraryDependencies ++= Seq(
        "com.google.caliper"   %  "caliper"         % "0.5-rc1")
      )
  )
  .dependsOn(vault)

}
