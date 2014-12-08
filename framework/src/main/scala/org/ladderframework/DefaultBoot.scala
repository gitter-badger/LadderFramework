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
	implicit lazy val executionContext = ExecutionContext.Implicits.global 
	
	def site:PartialFunction[HttpRequest, Future[HttpResponse]]
	
	def notFound:HttpResponse = NotFoundDefaultResponse
	def error(status: Status, ot: Option[Throwable]): HttpResponse = ErrorResponse(status, ot)
	
	def errorHandle:PartialFunction[(Status, Option[Throwable]), HttpResponse] = {
		case (NotFound, _) => notFound
		case (s, ot) => error(s, ot)
	}
	
	def onShutdown()={}
	
	val timeToLivePage: Int = 10 * 60 * 1000
	
	//def resourcePath(resource:String):URL = getClass().getClassLoader().getResource(resource) 
	
	def resource(s:String):URL = resourceImpl(s) 
	def resourceAsStream(s:String):InputStream = resourceAsStreamImpl(s) 
	def mimeType(s:String):String = mimeTypeImpl(s)
	def contextPath:String = contextPathImpl
	
	private[ladderframework] val resourceImpl:String => URL = s => new File(s).toURI.toURL
	private[ladderframework] val resourceAsStreamImpl:String => InputStream = s => new FileInputStream(new File(resource(s).toURI))
	private[ladderframework] val mimeTypeImpl:String => String = s => s
	private[ladderframework] val contextPathImpl:String = ""
	
}