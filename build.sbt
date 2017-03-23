import play.PlayScala

name := """msviz-backend"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "mvnrepository" at "http://mvnrepository.com/artifact",
  //"expasy" at "http://mzjava.expasy.org/maven",
  "csvjdbc" at "http://csvjdbc.sourceforge.net/maven2",
  "netbeans" at "http://bits.netbeans.org/maven2/",
   "ebi-repo" at "http://www.ebi.ac.uk/~maven/m2repo",
   "genesis-maven2-repository" at "http://genesis.UGent.be/maven2",
   "com.springsource.repository.bundles.release" at "http://repository.springsource.com/maven/bundles/release",
   "com.springsource.repository.bundles.external" at "http://repository.springsource.com/maven/bundles/external"
  )

libraryDependencies ++= Seq(
  ws,
  cache,
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.specs2" %% "specs2" % "2.3.11" % "test",
 // "org.expasy.mzjava" % "mzjava-core" %"1.1.1-SNAPSHOT",
 // "org.expasy.mzjava" % "mzjava-proteomics" %"1.1.1-SNAPSHOT",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23",
  "com.wordnik" %% "swagger-play2" % "1.3.10",
  "org.scalamock" %% "scalamock-specs2-support" % "3.2",
  "net.lingala.zip4j" % "zip4j" % "1.3.2",
  "uk.ac.ebi.jmzml" % "jmzml" % "1.7.4",
  "net.sf.trove4j" % "trove4j" % "3.0.1",
  "colt" % "colt" % "1.2.0"
)

javaOptions in Test += "-Dconfig.file=conf/application.test.conf"