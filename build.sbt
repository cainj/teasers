import Dependencies._
import sbt.Keys._
import sbt._


lazy val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
lazy val mavenCentral = "Maven Central" at "http://central.maven.org"
lazy val performanceTest = config("performance") extend (Test)
lazy val codeCoverage = "Scala Code Coverage-github-repository" at "http://mtkopone.github.com/scct/maven-repo"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.2",
      resolvers ++= Seq(typesafe, mavenCentral, codeCoverage),
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Solver",
    libraryDependencies += scalaTest % Test
  ).settings(scalariformSettings: _*)

coverageMinimum := 70

coverageFailOnMinimum := false

coverageHighlighting := true
