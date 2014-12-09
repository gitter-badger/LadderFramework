import sbt._
import Keys._
import sbtrelease.ReleasePlugin._

object LadderFrameworkBuild extends Build {

	val buildScalaVersion = "2.11.4"
	
	lazy val buildSettings = Defaults.defaultSettings ++ Seq(
		organization := "org.ladderframework",
		publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))),
		scalaVersion := buildScalaVersion)

	lazy val root = Project(id = "root",
		base = file("."),
		settings = ladderSettings ++ releaseSettings
	) aggregate (framework, testFramework)

	lazy val framework = Project(id = "ladder-web",
		base = file("framework"),
		settings = ladderSettings ++ Seq(
			libraryDependencies ++= Dependencies.framework ++ Dependencies.testkit
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

	val testkit = Seq(Test.scalatest, Test.scalacheck, Test.akkaTest)

	val framework = Seq(akkaActor, akkaLogging, akkaHttp, scalaXml, scalaParserCompinators) ++ slf4j
	val test_framework = slf4j

}

object Dependency {

	// Versions

	object V {
		val logback = "1.0.13"
		val scalatest = "2.2.2"
		val scalacheck = "1.11.4"
		val slf4j = "1.7.5"
		val akka = "2.3.7"
		val scalaVersion = "2.11.4"
	}

	// Compile
	lazy val slf4jApi = "org.slf4j" % "slf4j-api" % V.slf4j
	lazy val logback = "ch.qos.logback" % "logback-classic" % V.logback

	lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
	lazy val scalaParserCompinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"

	lazy val akkaActor = "com.typesafe.akka" %% "akka-actor" % V.akka
	lazy val akkaLogging = "com.typesafe.akka" %% "akka-slf4j" % V.akka
	lazy val akkaHttp = "com.typesafe.akka" %% "akka-http-core-experimental" % "1.0-M1"

	object Test {
		val scalatest = "org.scalatest" %% "scalatest" % V.scalatest % "test" // ApacheV2
		val akkaTest = "com.typesafe.akka" %% "akka-testkit" % V.akka % "test"
		val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck % "test"
	}
}
