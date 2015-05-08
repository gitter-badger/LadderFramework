import sbt._
import Keys._
import sbtrelease.ReleasePlugin._

object LadderFrameworkBuild extends Build {

	val buildScalaVersion = "2.11.6"
	
	lazy val buildSettings = Defaults.defaultSettings ++ Seq(
		organization := "org.ladderframework",
		publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))),
		scalaVersion := buildScalaVersion)

	lazy val root = Project(id = "root",
		base = file("."),
		settings = ladderSettings ++ releaseSettings
	) aggregate (json, css, framework, testFramework)

	lazy val framework = Project(id = "ladder-web",
		base = file("framework"),
		settings = ladderSettings ++ Seq(
			libraryDependencies ++= Dependencies.framework ++ Dependencies.testkit ++ Seq(Dependency.Test.akkaTest)
		)
	) dependsOn (json, css)

	lazy val json = Project(id = "ladder-json",
		base = file("json"),
		settings = ladderSettings ++ Seq(
			libraryDependencies ++= Dependencies.json ++ Dependencies.testkit 
		)
	)

	lazy val css = Project(id = "ladder-css",
		base = file("css"),
		settings = ladderSettings ++ Seq(
			libraryDependencies ++= Dependencies.css ++ Dependencies.testkit
		)
	)

	lazy val testFramework = Project(id = "ladder-test",
		base = file("test-framework"),
		settings = ladderSettings ++ Seq(
			libraryDependencies ++= Dependencies.test_framework ++ Dependencies.testkit
		)
	) dependsOn (framework)

	lazy val ladderSettings = buildSettings ++ Seq(
		scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
		resolvers ++= Resolvers.ladderResolvers
	) 
}

object Resolvers {
  val typesafeRepo = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  val akkaRepo =  "Akka Repository" at "http://repo.akka.io/releases/"
  val ladderResolvers = Seq (typesafeRepo, akkaRepo)
}

object Dependencies {
	import Dependency._

	val slf4j = Seq(slf4jApi, logback)

	val testkit = Seq(Test.scalatest, Test.scalacheck, Test.junit)

	val framework = Seq(servletApi, websocketApi, jettyServer, jettyContinuation, akkaActor, akkaLogging, scalaXml, scalaParserCompinators) ++ slf4j
	val test_framework = Seq(servletApi) ++ slf4j
	val json = Seq(scalaParserCompinators)
	val css = Seq(scalaXml)

}

object Dependency {

	// Versions

	object V {
		val logback = "1.0.13"
		val scalatest = "2.2.0"
		val scalacheck = "1.11.4"
		val slf4j = "1.7.5"
		val akka = "2.3.7"
		val scalaVersion = "2.11.6"
	}

	// Compile
	lazy val jetty = "org.eclipse.jetty" % "jetty-webapp" % "9.2.7.v20150116" % "test"

	lazy val jettyContinuation = "org.eclipse.jetty" % "jetty-continuation" % "9.2.7.v20150116"

	lazy val servletApi = "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided"

	lazy val websocketApi = "javax.websocket" % "javax.websocket-api" % "1.0" % "provided"

	lazy val slf4jApi = "org.slf4j" % "slf4j-api" % V.slf4j
	lazy val logback = "ch.qos.logback" % "logback-classic" % V.logback

	lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
	lazy val scalaParserCompinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

	lazy val akkaActor = "com.typesafe.akka" %% "akka-actor" % V.akka
	lazy val akkaLogging = "com.typesafe.akka" %% "akka-slf4j" % V.akka

	lazy val jettyServer = "org.eclipse.jetty" % "jetty-server" % "9.2.7.v20150116"

	object Test {
		val junit = "junit" % "junit" % "4.11" % "test" // Common Public License 1.0
		val scalatest = "org.scalatest" %% "scalatest" % V.scalatest % "test" // ApacheV2
		val akkaTest = "com.typesafe.akka" %% "akka-testkit" % V.akka % "test"
		val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck % "test"
	}
}
