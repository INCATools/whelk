import scala.scalanative.build._

lazy val owlapiVersion = "4.5.22"

lazy val commonSettings = Seq(
  organization := "org.geneontology",
  version := "1.2.1",
  licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  homepage := Some(url("https://github.com/balhoff/whelk")),
  scalaVersion := "2.13.14",
  crossScalaVersions := Seq("2.13.14"),
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
)

lazy val publishSettings = Seq(
  Test / publishArtifact := false,
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
  Test / scalacOptions ++= Seq("-Yrangepos", "-feature"),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  libraryDependencies ++= Seq("com.lihaoyi" %%% "utest" % "0.7.11" % Test)
)

lazy val parentProject = project
  .in(file("."))
  .settings(commonSettings)
  .settings(name := "whelk-project", publish / skip := true)
  .aggregate(
    coreJVM,
    owlapi,
    protege
  )

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(testSettings)
  .settings(publishSettings)
  .settings(
    name := "whelk",
    description := "Whelk reasoner core",
    libraryDependencies ++= Seq("org.geneontology" %%% "archimedes" % "0.1.1")
  )
  .nativeSettings(
    nativeConfig ~= {
      _.withLTO(LTO.thin)
        .withMode(Mode.releaseFast)
        .withGC(GC.immix)
    }
  )

lazy val coreJVM = core.jvm.enablePlugins(JavaAppPackaging)

lazy val owlapi = project
  .in(file("modules/owlapi"))
  .dependsOn(core.jvm)
  .enablePlugins(JavaAppPackaging)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(publishSettings)
  .settings(
    name := "whelk-owlapi",
    description := "Whelk reasoner OWL API bindings",
    Compile / mainClass := Some("org.geneontology.whelk.Main"),
    libraryDependencies ++= Seq(
      "net.sourceforge.owlapi" % "owlapi-distribution" % owlapiVersion,
      "org.phenoscape" %% "scowl" % "1.4.1",
      "org.semanticweb.elk" % "elk-owlapi" % "0.4.3" % Test,
      "net.sourceforge.owlapi" % "org.semanticweb.hermit" % "1.4.0.432" % Test,
      "net.sourceforge.owlapi" % "jfact" % "4.0.4" % Test
    )
  )

def isJarToEmbed(file: java.io.File): Boolean = file.getName match {
  case name if (name startsWith "scala") || (name startsWith "scowl") => true
  case _                                                              => false
}

lazy val protege = project
  .in(file("modules/protege"))
  .dependsOn(owlapi)
  .enablePlugins(SbtOsgi)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    name := "Whelk reasoner Protege plugin",
    description := "Whelk reasoner Protégé plugin",
    // Bundle-Version is set to the version by default.
    OsgiKeys.bundleSymbolicName := "org.geneontology.whelk;singleton:=true",
    OsgiKeys.bundleActivator := Some("org.protege.editor.owl.ProtegeOWL"),
    // Include the packages specified by privatePackage in the bundle.
    OsgiKeys.privatePackage := Seq("org.geneontology.*"),
    OsgiKeys.exportPackage := Seq("!*"),
    OsgiKeys.importPackage := Seq("!sourcecode", "!org.geneontology.archimedes", "!fastparse", "!fastparse.*", "!org.hamcrest", "!sun.misc", "*", "sun.misc;resolution:=optional"),
    OsgiKeys.failOnUndecidedPackage := true,
    OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))"""",
    OsgiKeys.embeddedJars := (Compile / Keys.externalDependencyClasspath).value map (_.data) filter isJarToEmbed,
    //TODO
    OsgiKeys.additionalHeaders := Map(
      "Update-Url" -> "https://raw.githubusercontent.com/balhoff/whelk/master/modules/protege/update.properties"
    ),
    libraryDependencies ++= Seq(
      "net.sourceforge.owlapi" % "owlapi-distribution" % owlapiVersion % Provided,
      "edu.stanford.protege" % "protege-editor-core" % "5.6.5" % Provided,
      "edu.stanford.protege" % "protege-editor-owl" % "5.6.5" % Provided
    )
  )

Global / useGpg := false