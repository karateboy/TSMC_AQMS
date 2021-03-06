
name := """tsmc"""

version := "1.2.21"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  jdbc,  
  cache,
  ws,
  "org.scalikejdbc" %% "scalikejdbc"                  % "2.5.2",
  "org.scalikejdbc" %% "scalikejdbc-config"           % "2.5.2",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" % "2.5.1",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "io.github.cloudify" %% "spdf" % "1.3.1"
)
mappings in Universal ++=
  (baseDirectory.value / "report_template" * "*" get) map
    (x => x -> ("report_template/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "notification" * "*" get) map
    (x => x -> ("notification/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "importEPA" * "*" get) map
    (x => x -> ("importEPA/" + x.getName))
        
mappings in Universal ++= 
 List(file("public/css/bootstrap.min.css") -> "public/css/bootstrap.min.css",
 	file("public/css/aqm.css") -> "public/css/aqm.css"
 )

PlayKeys.fileWatchService := play.runsupport.FileWatchService.sbt(2000)

scalacOptions += "-feature"

routesGenerator := InjectedRoutesGenerator

fork in run := false
