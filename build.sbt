name := "rikai"
ThisBuild / organization := "org.fkie"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.3.2"

val cpgVersion = "1.6.2"
val joernVersion = "2.0.252"
val overflowdbVersion = "1.184"

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.9.2",
  "com.github.scopt" %% "scopt" % "4.1.0",
  //"org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.23.0" % Runtime,
  "ch.qos.logback" % "logback-classic" % "1.5.0",
  "com.typesafe" % "config" % "1.3.3",                // config file parsing
  "com.vaticle.typedb" % "typedb-driver" % "2.26.5" exclude("ch.qos.logback", "*"),
  "io.joern" %% "x2cpg" % Versions.joern,
  "io.joern" %% "c2cpg" % Versions.joern,
  "io.joern" %% "joern-cli" % Versions.joern,
  "io.joern" %% "semanticcpg" % Versions.joern,
  "io.joern" %% "semanticcpg" % Versions.joern % Test classifier "tests",
  "org.scalatest" %% "scalatest" % "3.2.10" % Test
)

ThisBuild/Compile/scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:implicitConversions",
)

enablePlugins(JavaAppPackaging)

ThisBuild/licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

Global/onChangedBuildSource := ReloadOnSourceChanges

ThisBuild/resolvers ++= Seq(
  Resolver.mavenLocal,
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
  "repo.vaticle.com" at "https://repo.vaticle.com/repository/maven/",
  "Gradle Releases" at "https://repo.gradle.org/gradle/libs-releases/"
)

Compile / doc / sources := Seq.empty
Compile / packageDoc / publishArtifact := false

Compile / unmanagedSourceDirectories += baseDirectory.value / "main/resources"