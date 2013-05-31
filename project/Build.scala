import sbt._
import Keys._
import sbtrelease.ReleasePlugin._

object LadderFrameworkBuild extends Build {

	val buildScalaVersion = "2.10.1"
	
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

	val testkit = Seq(Test.scalatest, Test.junit, Test.akkaTest)

	val framework = Seq(scalaActor, servletApi, akkaActor, akkaLogging, json4sJackson) ++ slf4j
	val test_framework = Seq(scalaActor, servletApi) ++ slf4j

}

object Dependency {

	// Versions

	object V {
		val logback = "1.0.7"
		val scalatest = "2.0.M6-SNAP5"
		val slf4j = "1.6.4"
		val akka = "2.1.4"
		val scalaVersion = "2.10.1"
	}

	// Compile
	lazy val jetty = "org.eclipse.jetty" % "jetty-webapp" % "9.0.0.RC0" % "test,container"

	lazy val servletApi = "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided"

	lazy val slf4jApi = "org.slf4j" % "slf4j-api" % V.slf4j
	lazy val logback = "ch.qos.logback" % "logback-classic" % V.logback

	lazy val scalaActor = "org.scala-lang" % "scala-actors" % V.scalaVersion % "test"
	lazy val scalaReflect = "prg.scala-lang" % "scala-reflect" % V.scalaVersion % "test"

	lazy val akkaActor = "com.typesafe.akka" %% "akka-actor" % V.akka
	lazy val akkaLogging = "com.typesafe.akka" %% "akka-slf4j" % V.akka

	lazy val json4sJackson = "org.json4s" %% "json4s-jackson" % "3.2.4"

	object Test {
		val junit = "junit" % "junit" % "4.10" % "test" // Common Public License 1.0
		val scalatest = "org.scalatest" %% "scalatest" % V.scalatest % "test" // ApacheV2
		val akkaTest = "com.typesafe.akka" %% "akka-testkit" % V.akka % "test"
	}
}
