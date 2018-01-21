enablePlugins(ScalaJSPlugin)

name := "watch_balances"

version := "0.1"

scalaVersion := "2.11.6"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

// lib
libraryDependencies += "io.argonaut" %%% "argonaut" % "6.2"
libraryDependencies += "fr.hmil" %%% "roshttp" % "2.1.0"

// frontend
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.4"
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.2"

// test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

// js deps
skip in packageJSDependencies := false
jsDependencies +=
  "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"
