import play.sbt.PlayImport._
import sbt._

version := "1.0-SNAPSHOT"

lazy val compileDeps = Seq(
  ws,
  guice
)

def test(scope: String) = Seq(
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % scope,
  "org.scalatest" %% "scalatest" % "3.0.5" % scope,
  "org.scalamock" %% "scalamock" % "4.1.0" % scope
)

lazy val root = (project in file("."))
    .settings(name := "ddcw-technical-test",
      organization := "com.hmrc",
      scalaVersion := "2.12.12",
      libraryDependencies ++= compileDeps ++ test("test"),
      PlayKeys.playDefaultPort := 1235)
  .enablePlugins(PlayScala)
