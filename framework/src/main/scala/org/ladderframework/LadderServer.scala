package org.ladderframework

import akka.http.Http
import akka.http.Http.Bind
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
import javax.servlet.http.Part


class LadderServer(boot: DefaultBoot) {

	implicit val askTimeout: Timeout = 5000.millis
	private implicit val system = boot.system
	private implicit val materializer = FlowMaterializer()
	import system.dispatcher

	def start(interface: String, port: Int): Unit = {
		val bindingFuture = IO(Http) ? Http.Bind(interface = "localhost", port = 8080)
		bindingFuture foreach {
			case Http.ServerBinding(localAddress, connectionStream) ⇒
				Flow(connectionStream).foreach({
					case Http.IncomingConnection(remoteAddress, requestProducer, responseConsumer) ⇒
						println("Accepted new connection from " + remoteAddress)

						def rh: AkkaHttpRequest => Future[AkkaHttpResponse] = req => {
							val prom = Promise[AkkaHttpResponse]()
							val asyncHandler = new AkkaAsyncRequestHandler(prom)
							
							new HttpInteraction(asyncHandler, new AkkaHttpRequestWrapper(req, sessionID = ???), ???)
							prom.future
						}
						
						def receive: PartialFunction[Any, Unit] = {
							case hi: HttpInteraction =>
								//log.debug("receive - HttpInteraction: " + hi)
								val sessionID = hi.req.sessionID
								// If request to existing find actor. Find and send
								val session = system.actorSelection(system / "user" / sessionID.value)
								session ! hi
							case ws: WsConnect =>
								//log.debug("receive - WsInteraction: " + ws)
								val sessionID = ws.sessionID
								val session = system.actorSelection(system / "user" / sessionID)
								session ! ws
						}
						
						Flow(requestProducer).mapFuture(rh).produceTo(responseConsumer)
				})
		}
	}

	def stop(): Unit = {

	}

}

class AkkaAsyncRequestHandler(pom: Promise[AkkaHttpResponse])(implicit ex: ExecutionContext) extends AsyncRequestHandler{
	def addListeners(onCompleteCallback: () => Unit, onErrorCallback: () => Unit, onStartCallback: () => Unit, onTimeoutCallback: () => Unit): Unit = {
		onStartCallback()
		pom.future.onComplete{
			case Success(s) => 
				onCompleteCallback()
			case Failure(t: TimeoutException) => 
				onTimeoutCallback()
			case Failure(_) => 
				onErrorCallback()
		}
	}
	def complete(): Unit = {
		pom.complete(???)
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
	def path:List[String] = ???
	def parameters: Map[String,Array[String]] = ???
	//TODO S wrap Part in something appropriate
	override def parts: List[Part] = ??? //req.entity.???
	override def part(name: String): Option[Part] = parts.filter{_.getName() == name}.headOption
	override def partAsString(name: String): Option[String] = part(name).map(part => Context.stream2String(part.getInputStream))
	def cookies: Seq[Cookie] = req.headers.collect{case c: akka.http.model.headers.Cookie => c}.flatMap(c => c.cookies).map(toLadderCookie)
	def invalidateSession(): Unit = {???}
	
	def toLadderCookie(akkaCookie: akka.http.model.headers.HttpCookie):Cookie = {
		import akkaCookie._
		Cookie(name, value, domain, akkaCookie.path, secure, maxAge.map(_.toInt).getOrElse(0), extension, httpOnly)
	}
}