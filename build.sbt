lazy val commonSettings = Seq(
  organization := "com.mindwidgets",
  version := "0.1.0",
  scalaVersion := "2.12.0",
  coverageEnabled := true
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "scratch",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
  )
