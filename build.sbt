
organization  := "org.geneontology"

name          := "owl-to-rules"

version       := "0.0.1-SNAPSHOT"

scalaVersion  := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += "Phenoscape Maven repository" at "http://phenoscape.svn.sourceforge.net/svnroot/phenoscape/trunk/maven/repository"

libraryDependencies ++= {
  Seq(
    "net.sourceforge.owlapi"      %  "owlapi-distribution" % "4.2.7",
    "org.apache.jena"             %  "apache-jena-libs"    % "3.1.1" pomOnly(),
    "org.phenoscape"              %% "scowl"               % "1.2",
    "com.typesafe.scala-logging"  %% "scala-logging"       % "3.4.0",
    "ch.qos.logback"              %  "logback-classic"     % "1.1.7",
    "org.codehaus.groovy"         %  "groovy-all"          % "2.4.6"
  )
}
