
name := """epa"""

version := "1.1.15"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  jdbc,  
  cache,
  ws,
  "org.scalikejdbc" %% "scalikejdbc"                  % "2.5.0",
  "org.scalikejdbc" %% "scalikejdbc-config"           % "2.5.0",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" % "2.5.1",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0"
)

mappings in Universal ++=
  (baseDirectory.value / "report_template" * "*" get) map
    (x => x -> ("report_template/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "export/hour" * "*" get) map
    (x => x -> ("export/hour/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "export/minute" * "*" get) map
    (x => x -> ("export/minute/" + x.getName))

PlayKeys.fileWatchService := play.runsupport.FileWatchService.sbt(2000)

scalacOptions += "-feature"

routesGenerator := InjectedRoutesGenerator

fork in run := false
