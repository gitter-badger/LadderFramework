package org.ladderframework

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

import org.ladderframework.logging.Loggable

import akka.http.Http
import akka.http.model.HttpMethods
import akka.http.model.{HttpRequest => AkkaHttpRequest}
import akka.http.model.{HttpResponse => AkkaHttpResponse}
import akka.stream.FlowMaterializer
import akka.stream.scaladsl.Flow
import akka.util.Timeout
import akka.util.Timeout.durationToTimeout

class LadderServer(boot: DefaultBoot) extends Loggable{

	implicit val askTimeout: Timeout = 5000.millis
	private implicit val system = boot.system
	private implicit val materializer = FlowMaterializer()
	import system.dispatcher
	
	def start(interface: String, port: Int): Unit = {
		val serverBinding = Http().bind(interface, port)
		serverBinding.connections foreach (connection => {
			info("Accepted new connection from " + connection.remoteAddress)
			def requestHandler: AkkaHttpRequest => Future[AkkaHttpResponse] = req => {
				val response = Promise[AkkaHttpResponse]()
				debug(s"handlre request $req")
			  system.actorOf(RequestHandler.create(boot, req, response))
			  response.future
			}
			connection.handleWithAsyncHandler(requestHandler)
		})
	}

	def stop(): Unit = {
			system.shutdown()
	}

}

case class AkkaHttpRequestWrapper(req: AkkaHttpRequest, val sessionId: SessionId) extends HttpRequest{
	def method:Method = {
		import akka.http.model.HttpMethods
		req.method match {
			case HttpMethods.GET => Method.GET
			case HttpMethods.CONNECT => Method.CONNECT
			case HttpMethods.DELETE => Method.DELETE
			case HttpMethods.HEAD => Method.HEAD
			case HttpMethods.OPTIONS => Method.OPTIONS
			case HttpMethods.PATCH => Method.PATCH
			case HttpMethods.POST => Method.POST
			case HttpMethods.PUT => Method.PUT
		}
	}
	override def headers: String => Option[String] = s => req.headers.find(_.name == s).map(_.value)
	def path:List[String] = req.uri.path.toString.split("/").filterNot(_.isEmpty).toList
	def parameters: Map[String,List[String]] = req.uri.query.toMultiMap
	//TODO S wrap Part in something appropriate
	req.entity.contentType.mediaType.isMultipart
//	req.entity.transformDataBytes(transformer)
	override def parts: List[Part] = ??? //req.entity.???
	override def part(name: String): Option[Part] = parts.filter{_.name == name}.headOption
	override def partAsString(name: String): Option[String] = part(name).map(part => Context.stream2String(part.content))
	def cookies: Seq[Cookie] = req.headers.collect{case c: akka.http.model.headers.Cookie => c}.flatMap(c => c.cookies).map(Cookie(_))
	
}