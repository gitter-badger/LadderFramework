package org.ladderframework

import java.util.UUID
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.ladderframework.js.JsCmd
import akka.actor.Actor
import akka.actor.ActorIdentity
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.ActorSelection.toScala
import akka.actor.Identify
import akka.actor.PoisonPill
import akka.actor.Props
import akka.actor.Terminated
import akka.actor.actorRef2Scala
import akka.http.model.{ContentType => AkkaContentType}
import akka.http.model.HttpCharset
import akka.http.model.HttpEntity
import akka.http.model.{HttpRequest => AkkaHttpRequest}
import akka.http.model.{HttpResponse => AkkaHttpResponse}
import akka.http.model.{MediaType => AkkaMediaType}
import akka.http.model.StatusCode.int2StatusCode
import akka.http.model.headers.{Cookie => AkkaCookie}
import akka.http.model.headers.HttpCookie
import akka.http.model.headers.`Set-Cookie`
import akka.stream.FlowMaterializer
import akka.stream.scaladsl.Source
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import java.io.BufferedReader
import java.io.BufferedInputStream
import scala.collection.BufferedIterator
import akka.util.ByteIterator
import java.io.InputStreamReader
import akka.util.ByteString
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption
import java.nio.MappedByteBuffer
import scala.util.control.NonFatal
import akka.http.model.StatusCodes
import akka.http.model.HttpEntity.ChunkStreamPart
import akka.http.model.MediaTypes
import scala.collection.Iterator


case class HttpInteraction(req: HttpRequest, res: Promise[HttpResponseOutput])
case class RenderInital(res: Promise[HttpResponseOutput])
case class CreateSession(sessionId: String)
case class RemoveSession(sessionId: String)
case class AddResponse(path: List[String], uuid: String, response: HttpResponse) {}
case class PushMessage(id: String = Utils.uuid, message: String) {
	lazy val asJson = { """{"id":"""" + id + """", "message":"""" + message.replace("\"", "\\\"") + """"}""" }
}
case class Invalidate(session: SessionId)

//case class WsConnect(sessionID: String, page: String, lastId: String, actor: ActorRef)
//case class InitWsConnection(session: WsSession, httpSessionID: String)
//case class InitWsPage(page: ActorRef)
//case class WsCallMessage(msg: JObject)
case object Ping

object SessionMaster {
	def props(boot: DefaultBoot) = Props(new SessionMaster(boot))
}

class SessionMaster(boot: DefaultBoot) extends Actor with ActorLogging{
	def receive = {
		case hi: HttpInteraction => 
			log.debug("New hi: {}", hi)
			context.child(hi.req.sessionId.value) match{
				case Some(ar) => ar ! hi
				case None => context.actorOf(SessionActor.apply(hi.req.sessionId, boot), hi.req.sessionId.value) ! hi
			}
		case i@Invalidate(session) =>
			log.debug("Invalidate session: {}", session)
			context.child(session.value) foreach( _ ! i)
	}
}

object RequestHandler{
	def create(boot: DefaultBoot, req: AkkaHttpRequest, res: Promise[AkkaHttpResponse])(implicit fm: FlowMaterializer): Props = Props(new RequestHandler(boot, req, res))
}

class RequestHandler(boot: DefaultBoot, req: AkkaHttpRequest, res: Promise[AkkaHttpResponse])(implicit fm: FlowMaterializer) extends Actor with ActorLogging{
	val chunkSize = 4096
	val httpResponseOutput = Promise[HttpResponseOutput]()
	
	var sessionId: SessionId = SessionId(Utils.secureRandom)
	def createSession(): Unit = {
		log.debug("Create session: {}", sessionId)
		boot.sessionMaster ! HttpInteraction(new AkkaHttpRequestWrapper(req, sessionId = sessionId), httpResponseOutput)
	}

	req.headers.collect{case c:AkkaCookie => c}.flatMap(_.cookies).filter(_.name == boot.sessionName).headOption.map(_.content) match {
		case None => 
			log.debug("No session in: {}", req)
			createSession()
		case Some(v) =>
			log.debug("Using session in: {}", req)
			sessionId = SessionId(v)
			context.actorSelection(context.system / "session" / sessionId.value) ! Identify(None)
	}
	
	httpResponseOutput.future.foreach(_ match {
		case hsro : HttpStringResponseOutput => 
			res.success(
				AkkaHttpResponse(
						status = hsro.status.code,
						headers = immutable.Seq(`Set-Cookie`(HttpCookie(boot.sessionName, sessionId.value))) ++ hsro.headers,
						entity = HttpEntity(
								AkkaContentType(AkkaMediaType.custom(hsro.contentType.mediaType.value), hsro.contentType.charset.map(c => HttpCharset.custom(c.name()))), 
								hsro.content
						)
				)
			)
		case hpro : HttpPathResponseOutput => 
			res.tryComplete(
				Try{
					val mappedByteBuffer = mmap(hpro.content)
	        val iterator = new ByteBufferIterator(mappedByteBuffer, chunkSize)
	        val chunks = Source(() => iterator).map(ChunkStreamPart.apply)
					AkkaHttpResponse(entity = HttpEntity.Chunked(AkkaMediaType.custom(hpro.contentType.mediaType.value), chunks))
	      } recover {
	        case NonFatal(cause) ⇒
	          log.error("Nonfatal error: cause = {}", cause.getMessage)
	          AkkaHttpResponse(StatusCodes.InternalServerError, entity = cause.getMessage)
	      }
	    )
		case hsro : HttpStreamResponseOutput =>
			val interator = new Iterator[Byte]{
				val is = new BufferedInputStream(hsro.content)
				var last = -1
				def hasNext = try{
					if(last != -1)
						true
					else{
						last = is.read()
						true
					}
				} catch {
					case _ : Throwable => false
				}
				def next(): Byte = {
					if(last != -1) {
						val r = last
						last = -1
						r.toByte
					} else {
						is.read().toByte
					}
				}
			}
				
			res.success(
				AkkaHttpResponse(
						status = hsro.status.code,
						headers = immutable.Seq(`Set-Cookie`(HttpCookie(boot.sessionName, sessionId.value))) ++ hsro.headers,
						entity = HttpEntity.Chunked(
								AkkaContentType(AkkaMediaType.custom(hsro.contentType.mediaType.value), hsro.contentType.charset.map(c => HttpCharset.custom(c.name()))),
								Source(() => interator).grouped(chunkSize).map(i => ByteString(i.map(_.toByte).toArray))
						)
				)
			)
		case other => 
			log.error("Unable to handle response output: {}", other)
			res.success(AkkaHttpResponse(StatusCodes.NotImplemented, entity = s"Not implemented: $other"))
	})
	
	def receive = {
	  case ActorIdentity(_, Some(actorRef)) =>
	  	log.debug("ActorIdentity: {}", actorRef)
	  	actorRef ! HttpInteraction(new AkkaHttpRequestWrapper(req, sessionId = sessionId), httpResponseOutput)
	  case ActorIdentity(_, None) => // not alive
	  	log.debug("ActorIdentity: None")
	  	createSession()
	}
	
	def mmap(path: java.nio.file.Path): MappedByteBuffer = {
    val channel = FileChannel.open(path, StandardOpenOption.READ)
    val result = channel.map(FileChannel.MapMode.READ_ONLY, 0L, channel.size())
    channel.close()
    result
  }
	
	class ByteBufferIterator(buffer:ByteBuffer, chunkSize:Int) extends Iterator[ByteString] {
	  require(buffer.isReadOnly)
	  require(chunkSize > 0)
	 
	  override def hasNext = buffer.hasRemaining
	 
	  override def next(): ByteString = {
	    val size = chunkSize min buffer.remaining()
	    val temp = buffer.slice()
	    temp.limit(size)
	    buffer.position(buffer.position() + size)
	    ByteString(temp)
	  }
	}
	
}

object SessionActor {
	def apply(sessionID: SessionId, boot: DefaultBoot): Props = Props(new SessionActor(sessionID, boot))
}

class SessionActor(sessionID: SessionId, boot: DefaultBoot) extends Actor with ActorLogging {
	def receive = {
		case hi: HttpInteraction =>
			// If request to existing find actor. Find and send
			hi.req.path match {
				case ("ajax" | "post" | "pull") :: id :: _ => //Match mer
					context.actorSelection(id) ! hi
				case _ if hi.req.parameters.headOption.flatMap(_._2.headOption).exists(_ == "redirect") =>
					log.debug("redirect")
					context.children.map(_.path).foreach(p => log.debug(p.toString))
					hi.req.parameters.headOption.map(_._1) match {
						case Some(id) =>
							log.debug("redirect:id = " + id)
							log.debug("child: " + context.child(id))
							context.child(id).getOrElse {
								val uuid: String = UUID.randomUUID.toString
								context.actorOf(InitalResponseContainer.props(self, hi.req, uuid, boot), name = uuid)
							} ! RenderInital(hi.res)
						case _ =>
							val uuid: String = UUID.randomUUID.toString
							val resonseContainerRef = context.actorOf(InitalResponseContainer.props(context.self, hi.req, uuid, boot), name = uuid)
							resonseContainerRef ! RenderInital(hi.res)
					}

				case _ =>
					val uuid: String = UUID.randomUUID.toString
					val resonseContainerRef = context.actorOf(InitalResponseContainer.props(context.self, hi.req, uuid, boot), name = uuid)
					resonseContainerRef ! RenderInital(hi.res)
			}
//		case ws: WsConnect =>
//			context.actorSelection(ws.page) ! ws
		case AddResponse(path, uuid, response) =>
			context.actorOf(RedirectResponseContainer.props(context.self, Promise().success(response), uuid, boot), name = uuid)
		case i@Invalidate(session) if sessionID == session =>
			context.system.scheduler.scheduleOnce(100 millis, self, PoisonPill)
			context.become({
				case _ => self ! PoisonPill 
			})
	}
}

object RedirectResponseContainer {
	def props(session: ActorRef, httpResponse: Promise[HttpResponse], uuid: String, boot: DefaultBoot): Props = Props(new RedirectResponseContainer(session, httpResponse, uuid, boot))
}

class RedirectResponseContainer(
	val session: ActorRef,
	val httpResponse: Promise[HttpResponse],
	val uuid: String,
	val boot: DefaultBoot) extends ResponseContainer

object InitalResponseContainer {
	def props(session: ActorRef, initalRequest: HttpRequest, uuid: String, boot: DefaultBoot): Props = Props(new InitalResponseContainer(session, initalRequest, uuid, boot))
}

class InitalResponseContainer(
		val session: ActorRef,
		val initalRequest: HttpRequest,
		val uuid: String,
		val boot: DefaultBoot) extends ResponseContainer {

	val httpResponse: Promise[HttpResponse] = Promise().completeWith((Future(boot.site).map(_ orElse notFound)).flatMap(_.apply(initalRequest)))

}

case object Tick

trait ResponseContainer extends Actor with ActorLogging {

	def boot: DefaultBoot
	def httpResponse: Promise[HttpResponse]
	val uuid: String
	def session: ActorRef
	private var messages: List[PushMessage] = Nil
	private var lastAccess = System.currentTimeMillis
	private var wsActors = Set[ActorRef]()

	var isTerminated = false

	private def updateLastAccess() = {
		log.debug("updateLastAccess from " + lastAccess )
		context.system.scheduler.scheduleOnce(boot.timeToLivePage millis, self, Tick)
		lastAccess = System.currentTimeMillis
	}

	override def preStart() = {
		updateLastAccess()
		isTerminated = false
	}

	override def postStop() = {
		log.debug("Closing Page: " + uuid)
		isTerminated = true
	}

	def addResponse(path: List[String], response: HttpResponse): String = {
		val uniqueID = UUID.randomUUID.toString
		session ! AddResponse(path, uniqueID, response)
		uniqueID
	}

	def notFound: PartialFunction[HttpRequest, Future[HttpResponse]] = {
		case r =>
			log.debug("Not found - request: " + r)
			log.debug("context: " + statefulContext)
			Future.successful(boot.notFound)
	}

	def errorHandle: PartialFunction[(Status, Option[Throwable]), HttpResponse] = {
		case (s, ot) => ErrorResponse(s, ot)
	}

	def update(message: JsCmd): Try[Unit] = {
		if (!isTerminated) {
			Try { (self ! PushMessage(message = message.toCmd)) }
		} else {
			Failure(new IllegalStateException("page is closed"))
		}
	}

	implicit val statefulContext = new Context(uuid, addResponse, update, boot)

	def receive = {
		case Tick =>
			log.debug("Tick: lastAccess: " + lastAccess + " left: " + (lastAccess - boot.timeToLivePage + System.currentTimeMillis))
			if (System.currentTimeMillis > lastAccess + boot.timeToLivePage) {
				self ! PoisonPill
			}
		case RenderInital(res) =>
			log.debug("RenderInital")
			updateLastAccess()

			def complete() = {
				log.debug("Completed")
				httpResponse.future.map(_ match {
					case _: Stateful =>
					case _ => self ! PoisonPill
				})
			}
			val future = httpResponse.future.flatMap(_.httpResponse())
			future.onComplete {
				case Failure(t) =>
					log.error(t, "Problems handling request ")
					(boot.errorHandle orElse errorHandle)((InternalServerError, Option(t))).httpResponse().onComplete {
						case resp => 
							res.complete(resp) 
							complete()
					}
				case shro @ Success(hro) =>
					log.debug("Status: " + hro.status + "; httpResponse: " + hro);
					res.complete(shro) 
					complete()
			}

		case newMessage: PushMessage =>
			log.debug("newMessage:" + newMessage)
			updateLastAccess()
			messages = messages :+ newMessage
			log.debug("messages:" + messages)
			context.children.foreach(_ ! newMessage)
		case HttpInteraction(request @ HttpRequest(_, "pull" :: `uuid` :: _), res) =>
			log.debug("pull")
			updateLastAccess()
			request.parameters.get("lastId").flatMap(_.headOption).foreach(id => {
				messages = messages.reverse.takeWhile(_.id != id).reverse
			})
			context.actorOf(PullActor(res, boot)) ! messages

		case HttpInteraction(req, res) =>
			updateLastAccess()
			val response: Future[HttpResponse] = try{
				(statefulContext.submitCallback orElse
				statefulContext.ajaxSubmitCallback orElse
				statefulContext.ajaxHandlerCallback orElse
				statefulContext.ajaxCallback orElse notFound).apply(req)
			}catch{
				case t: Throwable => Future.failed(t)
			}
			response.onComplete {
				case Success(http) =>
					log.debug("completing - success, http: " + http)
					val resp = http.httpResponse()
					resp.onComplete {
						case Success(hro) =>
							res.complete(Success(hro))
							log.debug("completed - success, status: " + hro.status)
						case Failure(throwable) =>
							log.error(throwable, "problem completing")
					}

				case Failure(fail) => {
					log.error("complete - fail: " + fail)
					ErrorResponse(InternalServerError, Some(fail)).httpResponse().onComplete {
						case Success(httpRes) =>
							res.success(httpRes)
							log.debug("completed - fail, status: " + httpRes.status)
						case Failure(throwable) =>
							log.error("problem completing fail result", throwable)
					}
				}
			}
//		case ws @ WsConnect(_, `uuid`, lastId, wsActor) =>
//			log.debug("ws connect: " + ws)
//			updateLastAccess()
//			wsActors += wsActor
//			context.watch(wsActor)
//			wsActor ! InitWsPage(self)
//			messages = messages.reverse.takeWhile(_.id != lastId).reverse
//			wsActor ! messages
//		case WsCallMessage(msg) =>
			//TODO Handle callback
//			(statefulContext.submitCallback orElse
//				statefulContext.ajaxSubmitCallback orElse
//				statefulContext.ajaxHandlerCallback orElse
//				statefulContext.ajaxCallback).apply(msg)
		case Ping => 
			updateLastAccess()
		case Terminated(watched) => 
			wsActors -= watched
	}

}

object PullActor {
	def apply(res: Promise[HttpResponseOutput], boot: DefaultBoot) = Props(new PullActor(res, boot))
}

class PullActor(res: Promise[HttpResponseOutput], boot: DefaultBoot) extends Actor with ActorLogging {

	object TimeToClose

	context.system.scheduler.scheduleOnce(24000 millis, self, TimeToClose)

	def receive = {
		case TimeToClose =>
			send(Nil)
		case Nil =>
		case msg: PushMessage =>
			log.debug("push msg: {}", msg)
			send(List(msg))
		case msgs: List[_] =>
			log.debug("push msgs: {}", msgs)
			val pushMsgs: List[PushMessage] = msgs.collect({ case msg: PushMessage => msg })
			send(pushMsgs)
	}

	def send(msgs: List[PushMessage]): Unit = {
		val response = PullResponse(msgs)
		implicit val c: Context = new Context(Utils.uuid, (_, _) => "", _ => Success({}), boot)
		val selfActor = context.self
		response.httpResponse().onComplete {
			case Success(hsro) =>
				res.complete(Success(hsro))
				log.debug("pull completed - success, output: {}", hsro)
				selfActor ! PoisonPill
			case Failure(throwable) =>
				res.complete(Failure(throwable))
				log.warning("pull problem completing fail result", throwable)
				selfActor ! PoisonPill
		}
	}
}

//object WsActor{
//	def props: Props = Props[WsActor]()
//	case object Ping
//}
//
//class WsActor() extends Actor with ActorLogging {
//	
//	def receive = {
//		case InitWsConnection(wsSession, httpSessionId) => 
//			context.become(connected(wsSession, httpSessionId))
//			val page = wsSession.getPathParameters().get("page")
//			val lastId = wsSession.getPathParameters().get("lastId")
//			val session = context.system.actorSelection("user/" + httpSessionId)
//			session ! WsConnect(httpSessionId, page, lastId, self)
//	}
//	
//	def connected(session: WsSession, httpSessionId: String): Actor.Receive = {
//		def send(msgs: List[PushMessage]) = {
//			if (session.isOpen()){
//				val text = msgs.map(_.asJson).mkString("""{"messages":[""", ",", "]}")
//				try {
//					session.getBasicRemote.sendText(text)
//				} catch {
//					case ioe: IOException =>
//						log.warning("problems sending message: " + text, ioe)
//				}
//			}else{
//				self ! PoisonPill
//			}
//		}
//		
//		def ping() = {
//			if (session.isOpen()){
//				try {
//					session.getBasicRemote.sendPing(java.nio.ByteBuffer.wrap("LadderPing".getBytes()))
//				} catch {
//					case ioe: IOException =>
//						log.warning("problems sending ping", ioe)
//				}
//			}else{
//				self ! PoisonPill
//			}
//		}
//		
//		{
//			case InitWsPage(page) => 
//				session.addMessageHandler(new MessageHandler.Whole[String](){
//					override def onMessage(msg: String) = {
//						msg.toJson match {
//							case Success(json:JObject) =>
//								log.debug("new message: " + json)
//								page ! WsCallMessage(json)
//							case Success(s) =>
//								log.warning("Not a JObject: " + s)
//							case Failure(f) => 
//								log.warning("Unable to parse message", f)
//								
//						}
//					}
//				})
//				session.addMessageHandler(new MessageHandler.Whole[PongMessage](){
//					override def onMessage(msg: PongMessage) = {
//						page ! Ping
//					}
//				})
//				
//				context.system.scheduler.schedule(10 millis, 30 seconds, self, WsActor.Ping) //PING PONG???
//				
//				context.become({
//					case Ping => 
//						ping()
//					case Nil =>
//					case msg: PushMessage =>
//						send(List(msg))
//					case msgs: List[_] =>
//						val pushMsgs: List[PushMessage] = msgs.collect({ case msg: PushMessage => msg })
//						send(pushMsgs)
//				})
//		}
//	}
//}

