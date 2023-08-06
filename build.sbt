ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2021"
  )

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "5.2.0" % "test")
