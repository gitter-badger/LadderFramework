import sbt._
import Keys._

object LadderFrameworkBuild extends Build {

	val buildScalaVersion = "2.10.0-RC1"
	
	lazy val buildSettings = Defaults.defaultSettings ++ Seq(
		organization := "org.ladderframework",
		version := "0.0.1-SNAPSHOT",
		scalaVersion := buildScalaVersion)

	lazy val root = Project(id = "root",
		base = file(".")
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

	val framework = Seq(scalaActor, servletApi, akkaActor, akkaLogging, scalaz) ++ slf4j
	val test_framework = Seq(scalaActor, servletApi) ++ slf4j

}

object Dependency {

	// Versions

	object V {
		val logback = "1.0.7"
		val scalatest = "2.0.M4"
		val slf4j = "1.6.4"
		val akka = "2.1.0-RC1"
		val scalaz = "7.0.0-M3"
		val scalaVersion = "2.10.0-RC1"
	}

	// Compile
	lazy val jetty = "org.eclipse.jetty" % "jetty-webapp" % "8.0.4.v20111024" % "test,container"

	lazy val servletApi = "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided"

	lazy val slf4jApi = "org.slf4j" % "slf4j-api" % V.slf4j
	lazy val logback = "ch.qos.logback" % "logback-classic" % V.logback

	lazy val scalaActor = "org.scala-lang" % "scala-actors" % V.scalaVersion % "test"
	lazy val scalaReflect = "prg.scala-lang" % "scala-reflect" % V.scalaVersion % "test"

	lazy val akkaActor = "com.typesafe.akka" % "akka-actor_2.10.0-RC1" % V.akka
	lazy val akkaLogging = "com.typesafe.akka" % "akka-slf4j_2.10.0-RC1" % V.akka

	lazy val scalaz = "org.scalaz" % "scalaz-core_2.10.0-M7" % V.scalaz	

	object Test {
		val junit = "junit" % "junit" % "4.10" % "test" // Common Public License 1.0
		val scalatest = "org.scalatest" % "scalatest_2.10.0-RC1" % V.scalatest % "test" // ApacheV2
		val akkaTest = "com.typesafe.akka" % "akka-testkit_2.10.0-RC1" % V.akka % "test"
	}
}
