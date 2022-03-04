
ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"

ThisBuild / libraryDependencies ++= Dependencies.ScalaTest.dependencies

lazy val root = (project in file("."))
  .aggregate(janken)
  .settings(
    ProjectConfig.RootProject.toSettings
  )

lazy val janken = (project in file("./janken"))
  .configure(prj => {
    val conf = ProjectConfig.SubProject("janken")
    val jigConfig = JigConfig(prj)

    prj.settings(
      conf.toSettings,
      jigConfig.defaultSettings,
      jig / jigPatternDomain := s".+\\.janken\\..+"
    )
  })
