name := "watch_balances"

version := "0.1"

scalaVersion := "2.11.6"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies += "fr.acinq" % "bitcoin-lib_2.11" % "0.9.14"
libraryDependencies += "io.argonaut" %% "argonaut" % "6.2"
libraryDependencies += "fr.hmil" %% "roshttp" % "2.1.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
