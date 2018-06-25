enablePlugins(JavaAppPackaging)

organization  := "org.geneontology"

name          := "whelk"

version       := "0.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/balhoff/whelk"))

scalaVersion  := "2.12.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.geneontology.whelk.Main")

javaOptions += "-Xmx20G"

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scalaz"                  %% "scalaz-core"            % "7.2.24",
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.5.2",
    "org.phenoscape"              %% "scowl"                  % "1.3.1",
//    "org.backuity.clist"          %% "clist-core"             % "3.5.0",
//    "org.backuity.clist"          %% "clist-macros"           % "3.5.0" % "provided",
//    "com.typesafe.scala-logging"  %% "scala-logging"          % "3.9.0",
    "ch.qos.logback"              %  "logback-classic"        % "1.2.3",
    "org.codehaus.groovy"         %  "groovy-all"             % "2.4.6",
//    "io.suzaku"                   %% "boopickle"              % "1.3.0",
    "org.semanticweb.elk"         %  "elk-owlapi"             % "0.4.3", // % Test,
  )
}
