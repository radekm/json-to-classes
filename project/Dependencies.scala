import sbt._

object Dependencies {
  lazy val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-parser"
  ).map(_ % "0.13.0")
  lazy val scalaMeta = "org.scalameta" %% "scalameta" % "4.3.10"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.1.1"
}
