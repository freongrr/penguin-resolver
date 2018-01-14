name := "penguin-resolver-root"

lazy val commonSettings = Seq(
  organization := "com.github.freongrr",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.4"
)

lazy val lib = (project in file("lib"))
  .settings(commonSettings)

lazy val web = (project in file("web"))
  .settings(commonSettings)
  .dependsOn(lib)
