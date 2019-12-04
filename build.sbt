enablePlugins(JavaAppPackaging)

organization  := "org.geneontology"

name          := "whelk"

version       := "0.4"

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

homepage := Some(url("https://github.com/balhoff/whelk"))

scalaVersion  := "2.13.1"

crossScalaVersions := Seq("2.12.10", "2.13.1")

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.geneontology.whelk.Main")

javaOptions += "-Xmx20G"

testFrameworks += new TestFramework("utest.runner.Framework")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scalaz"             %% "scalaz-core"            % "7.2.28",
    "net.sourceforge.owlapi" %  "owlapi-distribution"    % "4.5.10",
    "org.phenoscape"         %% "scowl"                  % "1.3.4",
    "org.semanticweb.elk"    %  "elk-owlapi"             % "0.4.3"     % Test,
    "net.sourceforge.owlapi" %  "org.semanticweb.hermit" % "1.4.0.432" % Test,
    "net.sourceforge.owlapi" %  "jfact"                  % "4.0.4"     % Test,
    "com.lihaoyi"            %% "utest"                  % "0.6.9"     % Test
  )
}

pomExtra := <scm>
              <url>git@github.com:balhoff/whelk.git</url>
              <connection>scm:git:git@github.com:balhoff/whelk.git</connection>
            </scm>
            <developers>
              <developer>
                <id>balhoff</id>
                <name>Jim Balhoff</name>
                <email>jim@balhoff.org</email>
              </developer>
            </developers>

