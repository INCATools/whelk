enablePlugins(JavaAppPackaging)

organization  := "org.geneontology"

name          := "whelk"

version       := "0.6.1"

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

crossScalaVersions := Seq("2.12.11", "2.13.2")

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.geneontology.whelk.Main")

javaOptions += "-Xmx20G"

testFrameworks += new TestFramework("utest.runner.Framework")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scalaz"             %% "scalaz-core"            % "7.3.2",
    "net.sourceforge.owlapi" %  "owlapi-distribution"    % "4.5.16",
    "org.phenoscape"         %% "scowl"                  % "1.3.4",
    "org.semanticweb.elk"    %  "elk-owlapi"             % "0.4.3"     % Test,
    "net.sourceforge.owlapi" %  "org.semanticweb.hermit" % "1.4.0.432" % Test,
    "net.sourceforge.owlapi" %  "jfact"                  % "4.0.4"     % Test,
    "com.lihaoyi"            %% "utest"                  % "0.7.4"     % Test
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

