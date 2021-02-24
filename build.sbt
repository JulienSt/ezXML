name := "ezXML"

ThisBuild / organization := "com.github.julienst"
ThisBuild / version := "0.8"
ThisBuild / scalaVersion := "2.13.5"
ThisBuild / scalacOptions += "-Ymacro-annotations"

lazy val core = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(
        name := "ezxml.core",
        libraryDependencies ++= Seq (
            "com.lihaoyi" %%% "fastparse" % "2.3.1", // https://mvnrepository.com/artifact/com.lihaoyi/fastparse
            "org.scala-lang.modules" %%% "scala-xml" % "1.3.0", // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
            "org.scalatest" %%% "scalatest" % "3.2.5" % Test // https://mvnrepository.com/artifact/org.scalatest/scalatest
        )
    )
    
lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val extension = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("extension"))
    .dependsOn(core)
    .settings(
        name := "ezxml.extension",
        libraryDependencies ++= Seq (
            "org.scala-lang.modules" %%% "scala-xml" % "1.3.0", // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
            "org.scala-lang" % "scala-reflect" % "2.13.5", // https://mvnrepository.com/artifact/org.scala-lang/scala-reflect
            "org.scalatest" %%% "scalatest" % "3.2.5" % Test // https://mvnrepository.com/artifact/org.scalatest/scalatest
        ),
        scalacOptions += "-Ymacro-annotations"
    )

lazy val extensionJVM = extension.jvm
lazy val extensionJS = extension.js

lazy val root = project.in(file(".")).aggregate(coreJVM, coreJS, extensionJVM, extensionJS)

ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / licenses := Seq("MIT" -> url("https://raw.githubusercontent.com/JulienSt/ezXML/master/LICENSE"))
ThisBuild / publishMavenStyle := true
ThisBuild / homepage := Some(url("https://github.com/julienst/ezxml"))
ThisBuild / scmInfo := Some(
    ScmInfo(
        url("https://github.com/julienst/ezxml"),
        "scm:https://github.com/JulienSt/ezXML.git"
        )
    )
ThisBuild / developers := List(
    Developer(id="com.github.julienst", name="Julien Stengel", email="julien.stengel@gmail.com", url=url("https://github.com/julienst"))
)