ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "se.randomserver"
ThisBuild / homepage := Some(url("https://github.com/randomserver/linear"))

lazy val CatsVersion = "2.8.0"
lazy val CatsEffectVersion = "3.3.12"
lazy val FS2Version = "3.2.12"

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
      "org.typelevel" %% "cats-core" % CatsVersion
    ),
    Compile / scalacOptions ++= Seq(
      "-Ykind-projector:underscores"
    )
  )

lazy val `live-graph` = (project in file("./live-graph"))
  .settings(
    name := "live-graph",
    idePackagePrefix := Some("se.randomserver"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"        % CatsVersion,
      "org.typelevel" %% "cats-effect"      % CatsEffectVersion,
      "co.fs2"        %% "fs2-core"         % FS2Version
    ),
  ).dependsOn(root)