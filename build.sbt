ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "threebit"
  )

libraryDependencies += "org.scalameta" %% "munit" % "1.2.4" % Test
