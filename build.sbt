name := "ezXML"

ThisBuild / organization := "com.github.julienst"
ThisBuild / version := "0.2"
ThisBuild / scalaVersion := "2.13.2"

lazy val core = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
//    .configs(IntegrationTest)
//    .settings(Defaults.itSettings: _*)
//    .jsSettings(inConfig(IntegrationTest)(ScalaJSPlugin.testConfigSettings): _*)
    .settings(
//        unmanagedSourceDirectories in IntegrationTest ++= CrossType.Full.sharedSrcDir(baseDirectory.value, "it").toSeq,
        name := "ezxml.core",
        libraryDependencies := Seq (
            "org.scala-lang.modules" %%% "scala-xml" % "1.3.0", // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
            "junit" % "junit" % "4.13" % Test, // https://mvnrepository.com/artifact/junit/junit
            "org.scalatest" %%% "scalatest" % "3.1.2" % Test // https://mvnrepository.com/artifact/org.scalatest/scalatest
        )
    )

lazy val extension = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("extension"))
    .dependsOn(core)
//    .configs(IntegrationTest)
//    .settings(Defaults.itSettings: _*)
//    .jsSettings(inConfig(IntegrationTest)(ScalaJSPlugin.testConfigSettings): _*)
    .settings(
//        unmanagedSourceDirectories in IntegrationTest ++= CrossType.Full.sharedSrcDir(baseDirectory.value, "it").toSeq,
        name := "ezxml.macros",
        libraryDependencies := Seq (
            "org.scala-lang.modules" %%% "scala-xml" % "1.3.0", // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
            "org.scala-lang" % "scala-reflect" % "2.13.2", // https://mvnrepository.com/artifact/org.scala-lang/scala-reflect
            "junit" % "junit" % "4.13" % Test, // https://mvnrepository.com/artifact/junit/junit
            "org.scalatest" %%% "scalatest" % "3.1.2" % Test // https://mvnrepository.com/artifact/org.scalatest/scalatest
        ),
        scalacOptions += "-Ymacro-annotations"
    )

lazy val root = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("."))
    .aggregate(core, extension)

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