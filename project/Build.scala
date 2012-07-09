import sbt._
import Keys._
import com.github.siasia.PluginKeys._
import com.github.siasia.WebPlugin._

object LadderFrameworkBuild extends Build {

	val buildScalaVersion = "2.9.2"
	
	lazy val buildSettings = Defaults.defaultSettings ++ Seq(
		organization := "org.ladderframework",
		version := "0.0.1-SNAPSHOT",
		scalaVersion := buildScalaVersion)

	lazy val root = Project(id = "root",
		base = file(".")
	) aggregate (framework, testFramework)

	lazy val framework = Project(id = "ladder_framework",
		base = file("framework"),
		settings = ladderSettings ++ Seq(
			libraryDependencies ++= Dependencies.framework ++ Dependencies.testkit
		)
	)

	lazy val testFramework = Project(id = "ladder_test-framework",
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

  val ladderResolvers = Seq (typesafeRepo)
}

object Dependencies {
	import Dependency._

	val slf4j = Seq(slf4jApi, logback)

	val testkit = Seq(Test.scalatest, Test.junit, Test.akkaTest)

	val framework = Seq(servletApi, akkaActor, akkaLogging) ++ slf4j
	val test_framework = Seq(servletApi) ++ slf4j

}

object Dependency {

	// Versions

	object V {
		val logback = "1.0.0"
		val scalatest = "1.7.1"
		val slf4j = "1.6.4"
		val akka = "2.0.2"
	}

	// Compile
	val jetty = "org.eclipse.jetty" % "jetty-webapp" % "8.0.4.v20111024" % "test,container"

	val servletApi = "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided"

	val slf4jApi = "org.slf4j" % "slf4j-api" % V.slf4j
	val logback = "ch.qos.logback" % "logback-classic" % V.logback

	val akkaActor = "com.typesafe.akka" % "akka-actor" % V.akka
	val akkaLogging = "com.typesafe.akka" % "akka-slf4j" % V.akka
	

	object Test {
		val junit = "junit" % "junit" % "4.10" % "test" // Common Public License 1.0
		val scalatest = "org.scalatest" %% "scalatest" % V.scalatest % "test" // ApacheV2
		val akkaTest = "com.typesafe.akka" % "akka-testkit" % V.akka % "test"
	}
}
