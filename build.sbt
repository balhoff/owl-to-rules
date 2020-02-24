
organization  := "org.geneontology"

name          := "owl-to-rules"

version       := "0.3.7"

publishMavenStyle := true

publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/balhoff/owl-to-rules"))

scalaVersion  := "2.13.1"

crossScalaVersions := Seq("2.12.10", "2.13.1")

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

javaOptions in Test += s"""-Djava.library.path=${baseDirectory.value / "native"}"""

fork in Test := true

// parallel collections are split into a separate package only in 2.13
def parDependency(scalaVersion: String) = CrossVersion.partialVersion(scalaVersion) match {
  case Some((2, 13)) => Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
  case _             => Seq.empty
}

libraryDependencies ++= {
  Seq(
    "net.sourceforge.owlapi"      %  "owlapi-distribution"        % "4.2.9",
    "org.apache.jena"             %  "apache-jena-libs"           % "3.13.1" pomOnly(),
    "org.phenoscape"              %% "scowl"                      % "1.3.4",
    "com.outr"                    %% "scribe-slf4j"               % "2.7.11",
    "net.sourceforge.owlapi"      %  "org.semanticweb.hermit"     % "1.3.8.413" % Test,
    "org.scalatest"               %% "scalatest"                  % "3.1.0" % Test
  )
}

libraryDependencies ++= parDependency(scalaVersion.value)

pomExtra := (
    <scm>
        <url>git@github.com:balhoff/owl-to-rules.git</url>
        <connection>scm:git:git@github.com:balhoff/owl-to-rules.git</connection>
    </scm>
    <developers>
        <developer>
            <id>balhoff</id>
            <name>Jim Balhoff</name>
            <email>jim@balhoff.org</email>
        </developer>
    </developers>
)
