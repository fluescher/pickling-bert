org.scalastyle.sbt.ScalastylePlugin.Settings

name := "pickling-erts"

version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
	"org.scala-lang" %% "scala-pickling" % "0.9.0-SNAPSHOT",
	"org.scalatest"  %% "scalatest" % "2.1.5" % "test",
	"org.scalacheck" %% "scalacheck" % "1.11.4" % "test"
	)

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
	Resolver.sonatypeRepo("releases"))

scalaVersion := "2.11.2"

scalacOptions ++= Seq(
		"-feature",
		"-deprecation")


// Run scalastyle checks when testing 

lazy val testScalaStyle = taskKey[Unit]("testScalaStyle")

testScalaStyle := {
	  org.scalastyle.sbt.PluginKeys.scalastyle.toTask("").value
}

(test in Test) <<= (test in Test) dependsOn testScalaStyle
