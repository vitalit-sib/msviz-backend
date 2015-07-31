import play.PlayScala

name := """msviz-backend"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.4"

resolvers ++= Seq(
  "mvnrepository" at "http://mvnrepository.com/artifact",
  "expasy" at "http://mzjava.expasy.org/maven",
  "csvjdbc" at "http://csvjdbc.sourceforge.net/maven2",
  "netbeans" at "http://bits.netbeans.org/maven2/"
 // "typesafe" at "http://repo.typesafe.com/typesafe/releases/"
  //  "ivy2 cache" at "file:// /Users/amasselo/.ivy2/cache"
  )

libraryDependencies ++= Seq(
  ws,
  cache,
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.specs2" %% "specs2" % "2.3.11" % "test",
  "org.expasy.mzjava" % "mzjava-core" %"1.1.1-SNAPSHOT",
  "org.expasy.mzjava" % "mzjava-proteomics" %"1.1.1-SNAPSHOT",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23",
  "com.wordnik" %% "swagger-play2" % "1.3.10",
  "org.scalamock" %% "scalamock-specs2-support" % "3.2"
 // "com.typesafe.slick" %% "slick" % "2.0.0",
 // "org.slf4j" % "slf4j-nop" % "1.6.4"
)

//ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := ".*Reverse.*;.*Filter.*"

//parallelExecution in Test := false
