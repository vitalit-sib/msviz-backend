import play.PlayScala

name := """msviz-backend"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "expasy" at "http://mzjava.expasy.org/maven",
  "csvjdbc" at "http://csvjdbc.sourceforge.net/maven2"
  )

libraryDependencies ++= Seq(
  ws,
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.specs2" %% "specs2" % "2.3.11" % "test",
  "org.expasy.mzjava" % "mzjava-core" %"1.0.0",
  "org.expasy.mzjava" % "mzjava-proteomics" %"1.0.0",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23"
)
