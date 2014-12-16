package org.ladderframework

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

import org.ladderframework.logging.Loggable

import akka.http.Http
import akka.http.model.{HttpRequest => AkkaHttpRequest}
import akka.http.model.{HttpResponse => AkkaHttpResponse}
import akka.stream.FlowMaterializer
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
				//debug(s"handlre request $req")
			  system.actorOf(RequestHandler.create(boot, req, response))
			  response.future
			}
			connection.handleWithAsyncHandler(requestHandler)
		})
	}

	def stop(): Unit = {
		if(!system.isTerminated){
			system.shutdown()
		}
	}

}
