name := "rikai"
ThisBuild/organization := "org.fkie"
ThisBuild/scalaVersion := "2.13.7"

// parsed by project/Versions.scala, updated by updateDependencies.sh
val cpgVersion = "1.3.537"
val joernVersion = "1.1.873"
val overflowdbVersion = "1.127"

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.9.1",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.17.1" % Runtime,
  "com.typesafe" % "config" % "1.3.3",                // config file parsing
  "com.vaticle.typedb" % "typedb-client" % "2.8.0",
  "io.joern" %% "x2cpg" % Versions.joern,
  "io.joern" %% "c2cpg" % Versions.joern,
  "io.joern" %% "joern-cli" % Versions.joern,
  "io.joern" %% "semanticcpg" % Versions.joern,
  "io.joern" %% "semanticcpg" % Versions.joern % Test classifier "tests",
  "org.scalatest" %% "scalatest" % "3.1.1" % Test
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
  "repo.vaticle.com" at "https://repo.vaticle.com/repository/maven/"
)

Compile / doc / sources := Seq.empty
Compile / packageDoc / publishArtifact := false

unmanagedSourceDirectories in Compile += baseDirectory.value / "main/resources"