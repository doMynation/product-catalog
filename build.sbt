name := "solariusAPI"

version := "1.1.3"

lazy val `solariusAPI` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

scalaVersion := "2.12.2"
scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(jdbc, ehcache, ws, specs2 % Test, guice, evolutions)
libraryDependencies += "com.h2database" % "h2" % "1.4.192"
libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.41"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

lazy val doobieVersion = "0.6.0"
libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-postgres" % doobieVersion,
  "org.tpolecat" %% "doobie-specs2" % doobieVersion
)

val tsecV = "0.0.1-M11"
libraryDependencies += "io.github.jmcardon" %% "tsec-common" % tsecV
libraryDependencies += "io.github.jmcardon" %% "tsec-password" % tsecV

unmanagedResourceDirectories in Test <+= baseDirectory(_ / "target/web/public/test")

mappings in Universal ++=
  (baseDirectory.value / "scripts" * "*" get) map
    (x => x -> ("scripts/" + x.getName))

