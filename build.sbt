import Dependencies._

ThisBuild / scalaVersion := "2.13.2"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "cz.radekm"

lazy val root = (project in file("."))
  .settings(
    name := "json-to-classes",
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
      scalaMeta,
      scalaTest % Test
    )
  )
