ThisBuild / scalaVersion := "3.3.7"
ThisBuild / name := "threebit"

lazy val root = (project in file("."))
  .settings(
    name := "threebit"
  )

libraryDependencies += "org.scalameta" %% "munit" % "1.2.4" % Test
