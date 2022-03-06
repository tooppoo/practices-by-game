
ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"

ThisBuild / libraryDependencies ++= Dependencies.ScalaTest.dependencies

lazy val root = (project in file("."))
  .aggregate(oldMaid)
  .settings(
    ProjectConfig.RootProject.toSettings
  )

lazy val oldMaid = (project in file("./old_maid"))
  .configure(prj => {
    val conf = ProjectConfig.SubProject("old_maid")
    val jigConfig = JigConfig(prj)

    prj.settings(
      conf.toSettings,
      jigConfig.defaultSettings,
      jig / jigPatternDomain := s".+\\.old_maid\\..+"
    )
  })
