name := "2021-may"

version := "0.1"

scalaVersion := "2.13.5"

lazy val libraries = List(
  "org.typelevel" %% "cats-effect" % "3.1.0",
  "com.disneystreaming" %% "weaver-cats" % "0.7.0" % Test
)

lazy val limitationGame = (project in file(".")).settings(
  libraryDependencies ++= libraries,
  testFrameworks += new TestFramework("weaver.framework.CatsEffect")
)