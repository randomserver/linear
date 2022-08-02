ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "se.randomserver"
ThisBuild / homepage := Some(url("https://github.com/randomserver/linear"))

lazy val root = (project in file("."))
  .enablePlugins(GitVersioning)
  .settings(
    name := "linear",
    idePackagePrefix := Some("se.randomserver"),
    git.useGitDescribe := true,
    git.gitTagToVersionNumber := { tag: String =>
      if (tag matches "[0-9]+\\..*") Some(tag)
      else None
    },
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0"
    )
  )
