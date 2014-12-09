package org.ladderframework

import akka.http.Http
import java.net.InetSocketAddress
import akka.io.IO
import akka.pattern.ask
import akka.stream.scaladsl.Flow
import akka.stream.FlowMaterializer
import akka.util.Timeout
import akka.util.Timeout._
import scala.concurrent.duration._
import scala.concurrent.Future
import akka.http.model.{HttpRequest => AkkaHttpRequest, HttpResponse => AkkaHttpResponse}
import javax.xml.ws.AsyncHandler
import scala.concurrent.Promise
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext
import akka.http.model.HttpHeader
import akka.http.model.headers.{Cookie => AkkaCookie}
import akka.http.model.headers.`Set-Cookie`
import org.ladderframework.logging.Loggable
import akka.http.model.headers.HttpCookie
import scala.collection.immutable
import akka.http.model.HttpEntity
import akka.http.model.{ContentType => AkkaContentType}
import akka.http.model.{MediaType => AkkaMediaType}
import akka.http.model.HttpCharset

class LadderServer(boot: DefaultBoot) extends Loggable{

	val sessionName = "LSession"
	
	implicit val askTimeout: Timeout = 5000.millis
	private implicit val system = boot.system
	private implicit val materializer = FlowMaterializer()
	import system.dispatcher

	def start(interface: String, port: Int): Unit = {
		val serverBinding = Http().bind(interface, port)
		serverBinding.connections foreach (connection => {
			println("Accepted new connection from " + connection.remoteAddress)
			def requestHandler: AkkaHttpRequest => Future[AkkaHttpResponse] = req => {
				debug("RH")
				val session = req.headers.collect{case c:AkkaCookie => c}.flatMap(_.cookies).filter(_.name == sessionName).headOption.map(_.value) match {
					case None => 
						val sessionId = SessionId(Utils.uuid.replace('-', 'X'))
						debug("Create session: " + sessionId)
						system.actorOf(SessionActor(sessionId, boot), name = sessionId.value)
						sessionId
					case Some(v) => SessionId(v)
				}
				val httpResponseOutput = Promise[HttpResponseOutput]()
				debug("next: interaction")
				val interaction = HttpInteraction(new AkkaHttpRequestWrapper(req, sessionID = session), httpResponseOutput)
				debug("next: handle interaction")
				receive(interaction)
				httpResponseOutput.future.map(_ match {
					case hsro : HttpStringResponseOutput => 
						AkkaHttpResponse(
								status = hsro.status.code,
								headers = immutable.Seq(`Set-Cookie`(HttpCookie(sessionName, session.value))) ++ hsro.headers,
								entity = HttpEntity(
										AkkaContentType(AkkaMediaType.custom(hsro.contentType.mediaType.value), hsro.contentType.charset.map(c => HttpCharset.custom(c.name()))), 
										hsro.content
								)
						)
				})
			}
			
			def receive: PartialFunction[Any, Unit] = {
				case hi: HttpInteraction =>
					info("receive - HttpInteraction: " + hi)
					val sessionID = hi.req.sessionID
					// If request to existing find actor. Find and send
					val session = system.actorSelection(system / sessionID.value)
					session ! hi
//				case ws: WsConnect =>
//					debug("receive - WsInteraction: " + ws)
//					val sessionID = ws.sessionID
//					val session = system.actorSelection(system / sessionID)
//					session ! ws
			}
			connection handleWith { Flow[AkkaHttpRequest] mapAsync requestHandler }
		})
	}

	def stop(): Unit = {
			system.shutdown()
	}

}

class AkkaHttpRequestWrapper(req: AkkaHttpRequest, val sessionID: SessionId) extends HttpRequest{
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
	override def parts: List[Part] = ??? //req.entity.???
	override def part(name: String): Option[Part] = parts.filter{_.name == name}.headOption
	override def partAsString(name: String): Option[String] = part(name).map(part => Context.stream2String(part.content))
	def cookies: Seq[Cookie] = req.headers.collect{case c: akka.http.model.headers.Cookie => c}.flatMap(c => c.cookies).map(Cookie(_))
	
}