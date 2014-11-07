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
  "org.specs2" %% "specs2" % "2.3.11" % "test",
  "org.expasy.mzjava" % "mzjava-core" %"1.0.0",
  "org.expasy.mzjava" % "mzjava-proteomics" %"1.0.0"
)
