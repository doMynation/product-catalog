name := "testscala26"

version := "1.0"

lazy val `testscala26` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

scalaVersion := "2.12.2"
scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(jdbc, ehcache, ws, specs2 % Test, guice, evolutions)
libraryDependencies += "com.h2database" % "h2" % "1.4.192"
libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.41"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

unmanagedResourceDirectories in Test <+= baseDirectory(_ / "target/web/public/test")
