enablePlugins(JavaAppPackaging)

lazy val scalazVersion = "7.3.2"

lazy val commonSettings = Seq(
  organization := "org.geneontology",
  version := "0.6.1",
  licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  homepage := Some(url("https://github.com/balhoff/whelk")),
  crossScalaVersions := Seq("2.12.11", "2.13.3"),
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
)

lazy val publishSettings = Seq(
  publishArtifact in Test := false,
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
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
)

lazy val testSettings = Seq(
  scalacOptions in Test ++= Seq("-Yrangepos", "-feature"),
  fork in Test := true,
  testFrameworks += new TestFramework("utest.runner.Framework"),
  libraryDependencies ++= Seq("com.lihaoyi" %% "utest" % "0.7.5" % Test)
)

lazy val parentProject = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "whelk-project",
    skip in publish := true)
  .aggregate(
    core,
    owlapi
  )

lazy val core = project.in(file("modules/core"))
  .settings(commonSettings)
  .settings(testSettings)
  .settings(publishSettings)
  .settings(
    name := "whelk",
    description := "Whelk reasoner core",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % scalazVersion
    )
  )

lazy val owlapi = project.in(file("modules/owlapi"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(publishSettings)
  .settings(
    name := "whelk-owlapi",
    description := "Whelk reasoner OWL API bindings",
    mainClass in Compile := Some("org.geneontology.whelk.Main"),
    libraryDependencies ++= Seq(
      "net.sourceforge.owlapi" % "owlapi-distribution" % "4.5.17",
      "org.scalaz" %% "scalaz-core" % scalazVersion,
      "org.phenoscape" %% "scowl" % "1.3.4",
      "org.semanticweb.elk" % "elk-owlapi" % "0.4.3" % Test,
      "net.sourceforge.owlapi" % "org.semanticweb.hermit" % "1.4.0.432" % Test,
      "net.sourceforge.owlapi" % "jfact" % "4.0.4" % Test
    )
  )
