ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "se.randomserver"
ThisBuild / homepage := Some(url("https://github.com/randomserver/linear"))

lazy val root = (project in file("."))
  .enablePlugins(GitVersioning)
  .settings(
    name := "linear",
    idePackagePrefix := Some("se.randomserver"),
    git.useGitDescribe := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0"
    )
  )
