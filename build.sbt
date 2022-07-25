ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "se.randomserver"
ThisBuild / homepage := Some(url("https://github.com/randomserver/linear"))

lazy val publishSettings = sys.env.get("GITHUB_TOKEN").map[Seq[Def.Setting[_]]] { password =>
  Seq(
    credentials += Credentials("GitHub Package Registry", "maven.pkg.github.com", "randomserver", password),
    publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/randomserver/vmath")
  )
}.getOrElse(Seq.empty[Def.Setting[_]])

lazy val root = (project in file("."))
  .enablePlugins(GitVersioning)
  .settings(
    name := "linear",
    idePackagePrefix := Some("se.randomserver"),
    git.useGitDescribe := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0"
    ),
    publishSettings
  )
