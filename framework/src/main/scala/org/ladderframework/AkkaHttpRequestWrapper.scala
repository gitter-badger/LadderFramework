package org.ladderframework

import scala.concurrent.Future
import scala.collection.Seq
import scala.collection.immutable.List
import akka.http.Http
import akka.http.model.HttpMethods
import akka.http.model.{HttpRequest => AkkaHttpRequest}
import akka.http.model.{HttpResponse => AkkaHttpResponse}
import akka.http.model.headers.`Content-Type`
import akka.stream.FlowMaterializer
import akka.stream.scaladsl.Flow
import akka.util.Timeout
import akka.util.Timeout.durationToTimeout
import akka.http.model.Multipart
import scala.concurrent.ExecutionContext
import akka.event.LoggingAdapter
import akka.event.NoLogging
import scala.concurrent.Promise
import akka.stream.scaladsl.Sink
import akka.http.model.MediaTypes

case class AkkaHttpRequestWrapper(req: AkkaHttpRequest, val sessionId: SessionId)(implicit ec: ExecutionContext, log: LoggingAdapter = NoLogging, flowMaterializer: FlowMaterializer) extends HttpRequest{
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
	override lazy val parts: Future[List[Part]] = {
		implicit val m = akka.http.unmarshalling.MultipartUnmarshallers.multipartFormDataUnmarshaller
		//import akka.http.unmarshalling.Unmarshal
		if(req.header[`Content-Type`].headOption.map(_.contentType.mediaType).exists(_ == MediaTypes.`multipart/form-data`)) {
			val promise = Promise[List[Part]]()
			promise.completeWith{
				req.entity.flatMap(_.parts.fold(List[Part]())((l, p) => l :+ Part(p))) 
			}.future
		} else {
			Future.successful(Nil)
		}
	}
	
	override def part(name: String): Future[Option[Part]] = parts.map(_.filter(_.name == name).headOption)
	private def partToContentString(part: Part): Future[Option[String]] =  part.content.fold("")(_ + _.mkString).map(Some(_))
	private val fos = Future.successful(None: Option[String])
	override def partAsString(name: String): Future[Option[String]] = part(name).flatMap(_.fold(fos)(partToContentString(_)))
	def cookies: Seq[Cookie] = req.headers.collect{case c: akka.http.model.headers.Cookie => c}.flatMap(c => c.cookies).map(Cookie(_))
	
}