lazy val owlapiVersion = "4.5.19"

lazy val commonSettings = Seq(
  organization := "org.geneontology",
  version := "1.0.4",
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

lazy val parentProject = project
  .in(file("."))
  .settings(commonSettings)
  .settings(name := "whelk-project", skip in publish := true)
  .aggregate(
    core,
    owlapi,
    protege
  )

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(testSettings)
  .settings(publishSettings)
  .settings(
    name := "whelk",
    description := "Whelk reasoner core"
  )

lazy val owlapi = project
  .in(file("modules/owlapi"))
  .dependsOn(core)
  .enablePlugins(JavaAppPackaging)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(publishSettings)
  .settings(
    name := "whelk-owlapi",
    description := "Whelk reasoner OWL API bindings",
    mainClass in Compile := Some("org.geneontology.whelk.Main"),
    libraryDependencies ++= Seq(
      "net.sourceforge.owlapi" % "owlapi-distribution"    % owlapiVersion,
      "org.phenoscape"        %% "scowl"                  % "1.3.4",
      "org.semanticweb.elk"    % "elk-owlapi"             % "0.4.3"     % Test,
      "net.sourceforge.owlapi" % "org.semanticweb.hermit" % "1.4.0.432" % Test,
      "net.sourceforge.owlapi" % "jfact"                  % "4.0.4"     % Test
    )
  )

def isJarToEmbed(file: java.io.File): Boolean = file.getName match {
  case name if (name startsWith "scala") || (name startsWith "scowl") => true
  case _ => false
}

lazy val protege = project
  .in(file("modules/protege"))
  .dependsOn(owlapi)
  .enablePlugins(SbtOsgi)
  .settings(commonSettings)
  .settings(
    skip in publish := true,
    name := "Whelk reasoner Protege plugin",
    description := "Whelk reasoner Protégé plugin",
    // Bundle-Version is set to the version by default.
    OsgiKeys.bundleSymbolicName := "org.geneontology.whelk;singleton:=true",
    OsgiKeys.bundleActivator := Some("org.protege.editor.owl.ProtegeOWL"),
    // Include the packages specified by privatePackage in the bundle.
    OsgiKeys.privatePackage := Seq("org.geneontology.*"),
    OsgiKeys.exportPackage := Seq("!*"),
    OsgiKeys.importPackage := Seq("!org.hamcrest", "!sun.misc", "*", "sun.misc;resolution:=optional"),
    OsgiKeys.failOnUndecidedPackage := true,
    OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))"""",
    OsgiKeys.embeddedJars := (Keys.externalDependencyClasspath in Compile).value map (_.data) filter isJarToEmbed,
    //TODO
    OsgiKeys.additionalHeaders := Map(
      "Update-Url" -> "https://raw.githubusercontent.com/balhoff/whelk/master/modules/protege/update.properties"
    ),
    libraryDependencies ++= Seq(
      "net.sourceforge.owlapi" % "owlapi-distribution" % owlapiVersion % Provided,
      "edu.stanford.protege"   % "protege-editor-core" % "5.5.0"       % Provided,
      "edu.stanford.protege"   % "protege-editor-owl"  % "5.5.0"       % Provided
    )
  )
