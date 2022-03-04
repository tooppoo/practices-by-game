import sbt._

object Dependencies {
  object ScalaTest {
    val version = "3.2.10"

    val dependencies = Seq(
      "org.scalatest" %% "scalatest" % version % Test,
      "org.scalatest" %% "scalatest-funspec" % "3.2.10" % Test
    )
  }
}
