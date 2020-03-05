name := "ezXML"

ThisBuild / organization := "jstengel"
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / libraryDependencies := Seq (
    "org.scala-lang.modules" %% "scala-xml" % "1.2.0", // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
    "junit" % "junit" % "4.12" % Test, // https://mvnrepository.com/artifact/junit/junit
    "org.scalatest" %% "scalatest" % "3.1.1" % Test // https://mvnrepository.com/artifact/org.scalatest/scalatest
    )

lazy val core = (project in file("core")).settings(name := "ezxml.core")
lazy val extension = (project in file("extension")).dependsOn(core).settings(
    name := "ezxml.extension",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.1", // https://mvnrepository.com/artifact/org.scala-lang/scala-reflect,
)

lazy val root = (project in file(".")).aggregate(core, extension)