package org.ladderframework

import java.net.URL
import java.io.File
import java.io.InputStream
import java.io.FileInputStream
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import akka.actor.PoisonPill

trait DefaultBoot {
	
	val akkaConfig = ConfigFactory.load()
	val system = ActorSystem("WebSystem", akkaConfig)
	
	lazy val sessionMaster = system.actorOf(SessionMaster.props(this), "session")
	implicit lazy val executionContext = ExecutionContext.Implicits.global 
	
	def site:PartialFunction[HttpRequest, Future[HttpResponse]]
	
	def notFound:HttpResponse = NotFoundDefaultResponse
	def error(status: Status, ot: Option[Throwable]): HttpResponse = ErrorResponse(status, ot)
	
	def errorHandle:PartialFunction[(Status, Option[Throwable]), HttpResponse] = {
		case (Status.NotFound, _) => notFound
		case (s, ot) => error(s, ot)
	}
	
	def invalidateSession(session: SessionId): Unit = {
		sessionMaster ! Invalidate(session)
	}
	
	def onShutdown()={}
	
	val timeToLivePage: Int = 10 * 60 * 1000
	
	//def resourcePath(resource:String):URL = getClass().getClassLoader().getResource(resource) 
	
	def resource(s:String):URL = resourceImpl(s) 
	def resourceAsStream(s:String):InputStream = resourceAsStreamImpl(s) 
	def mimeType(s:String):String = mimeTypeImpl(s)
	def contextPath:String = contextPathImpl
	
	private[ladderframework] var resourceImpl:String => URL = s => getClass.getResource(if(s.startsWith("/")) s else "/" + s)
	private[ladderframework] var resourceAsStreamImpl:String => InputStream = s => getClass.getResourceAsStream(if(s.startsWith("/")) s else "/" + s)
	private[ladderframework] var mimeTypeImpl:String => String = s => "unknown/type"
	private[ladderframework] var contextPathImpl:String = ""
	
}