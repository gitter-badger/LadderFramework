package org.ladderframework

import java.util.UUID
import scala.collection.immutable
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
import scala.collection.Iterator
import org.ladderframework.json.JObject
import java.io.IOException
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.HttpServletRequest
import scala.io.Source
import javax.websocket.{Session => WsSession}
import java.nio.file.Files
import javax.websocket.MessageHandler
import javax.websocket.PongMessage
import javax.websocket.PongMessage

case class HttpInteraction(req: HttpRequest, res: Promise[HttpResponseOutput])
case class RenderInital(res: Promise[HttpResponseOutput])
case class CreateSession(sessionId: String)
case class RemoveSession(sessionId: String)
case class AddResponse(path: List[String], uuid: String, response: HttpResponse) {}
case class PushMessage(id: String = Utils.uuid, message: String) {
	lazy val asJson = { """{"id":"""" + id + """", "message":"""" + message.replace("\"", "\\\"") + """"}""" }
}
case class Invalidate(session: SessionId)

case class WsConnect(sessionID: String, page: String, lastId: String, actor: ActorRef)
case class InitWsConnection(session: WsSession, httpSessionID: String)
case class InitWsPage(page: ActorRef)
case class WsCallMessage(msg: JObject)
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
	def create(boot: DefaultBoot, completed: () => Unit, req: HttpServletRequest, res: HttpServletResponse): Props = Props(new RequestHandler(boot, completed, req, res))
}

class RequestHandler(boot: DefaultBoot, completed: () => Unit, req: HttpServletRequest, res: HttpServletResponse) extends Actor with ActorLogging{
	import boot.executionContext
	val httpResponseOutput = Promise[HttpResponseOutput]()
	
	//var sessionId: SessionId = 
	val cookies = Option(req.getCookies()).map(_.toList).getOrElse(Nil).map(c => Cookie(c))
	def createSession(): ServletHttpRequest = {
		val sessionId = SessionId(Utils.secureRandom)
		log.debug("Create session: {}", sessionId)
		val request = new ServletHttpRequest(req, cookies, sessionId)
		boot.sessionMaster ! HttpInteraction(request, httpResponseOutput)
		request
	}
	
	val request = cookies.find(_.name == boot.sessionName).map(_.value) match {
		case None => 
			log.debug("No session in: {}", req)
			createSession()
		case Some(v) =>
			log.debug("Using session in: {}", req)
			val sessionId = SessionId(v)
			val request = new ServletHttpRequest(req, cookies, sessionId)
			context.actorSelection(context.system / "session" / sessionId.value) ! Identify(None)
			request
	}
	
	
	httpResponseOutput.future.map(i => {
			log.info("RES COMLETE: {}", i); 
			i
		}).map(_ match {
		case hsro : HttpStringResponseOutput => 
			log.info("hsro: {}", hsro )
			res.setStatus(hsro.status.code)
			hsro.headers.foreach{case HeaderValue(header, value) => res.addHeader(header.name, value)}
			res.setContentType(hsro.contentType.mediaType.value)
			hsro.contentType.charset.map(_.name)foreach(res.setCharacterEncoding)
			res.setContentLength(hsro.content.length)
			res.getOutputStream.print(hsro.content)
			res.getOutputStream.close()
			completed()
		case hpro : HttpPathResponseOutput => 
			res.setStatus(hpro.status.code)
			hpro.headers.foreach{case HeaderValue(header, value) => res.addHeader(header.name, value)}
			res.setContentType(hpro.contentType.mediaType.value)
			hpro.contentType.charset.map(_.name)foreach(res.setCharacterEncoding)
			res.setContentLengthLong(Files.size(hpro.content))
			val os = res.getOutputStream
			val size = Files.copy(hpro.content, os)
			os.close()
			completed()
		case hsro : HttpStreamResponseOutput =>
			res.setStatus(hsro.status.code)
			hsro.headers.foreach{case HeaderValue(header, value) => res.addHeader(header.name, value)}
			res.setContentType(hsro.contentType.mediaType.value)
			hsro.contentType.charset.map(_.name).foreach(res.setCharacterEncoding)
			res.setContentLength(hsro.size)
			val bufferSize = if(res.getBufferSize > 0) res.getBufferSize else 1024
			log.debug("Stream.bufferSize: {}", bufferSize)
			var buffer = new Array[Byte](bufferSize)
			val os = res.getOutputStream()
			val len = Iterator.continually(hsro.content.read(buffer)).takeWhile (_ > 0).map(read => {os.write(buffer,0,read); read}).sum			
			hsro.content.close()
			os.close()
			completed()
		case other => 
			log.error("Unable to handle response output: {}", other)
			res.setStatus(Status.NotImplemented.code)
			val os = res.getOutputStream()
			os.println(s"Unable to handle response output: $other")
			os.close()
			completed()
	}).foreach(_ => { 
		log.debug("Handled request")
		self ! PoisonPill
	})
	
	def receive = {
	  case ActorIdentity(_, Some(actorRef)) =>
	  	log.debug("ActorIdentity: {}", actorRef)
	  	actorRef ! HttpInteraction(request, httpResponseOutput)
	  case ActorIdentity(_, None) => // not alive
	  	log.debug("ActorIdentity: None")
	  	createSession()
	}
	
}

object SessionActor {
	def apply(sessionID: SessionId, boot: DefaultBoot): Props = Props(new SessionActor(sessionID, boot))
}

class SessionActor(sessionID: SessionId, boot: DefaultBoot) extends Actor with ActorLogging {
	import boot.executionContext
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
	import boot.executionContext
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
		implicit val ex = boot.executionContext
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
			implicit val ec = boot.executionContext
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
					(boot.errorHandle orElse errorHandle)((Status.InternalServerError, Option(t))).httpResponse().onComplete {
						case resp =>
							log.debug("UPS: " + uuid)
							res.complete(resp) 
							complete()
					}
				case shro @ Success(hro) =>
					log.debug("Status: " + hro.status + "; httpResponse: " + hro + " --: " + uuid);
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
			implicit val ex = boot.executionContext
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
					ErrorResponse(Status.InternalServerError, Some(fail)).httpResponse().onComplete {
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
//			//TODO Handle callback
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
	import boot.executionContext
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

object WsActor{
	def props(boot: DefaultBoot): Props = Props(new WsActor(boot))
	case object Ping
}

class WsActor(boot: DefaultBoot) extends Actor with ActorLogging {
	implicit val ex = boot.executionContext
	def receive = {
		case InitWsConnection(wsSession, httpSessionId) => 
			context.become(connected(wsSession, httpSessionId))
			val page = wsSession.getPathParameters().get("page")
			val lastId = wsSession.getPathParameters().get("lastId")
			val session = context.system.actorSelection("user/" + httpSessionId)
			session ! WsConnect(httpSessionId, page, lastId, self)
	}
	
	def connected(session: WsSession, httpSessionId: String): Actor.Receive = {
		def send(msgs: List[PushMessage]) = {
			if (session.isOpen()){
				val text = msgs.map(_.asJson).mkString("""{"messages":[""", ",", "]}")
				try {
					session.getBasicRemote.sendText(text)
				} catch {
					case ioe: IOException =>
						log.warning("problems sending message: " + text, ioe)
				}
			}else{
				self ! PoisonPill
			}
		}
		
		def ping() = {
			if (session.isOpen()){
				try {
					session.getBasicRemote.sendPing(java.nio.ByteBuffer.wrap("LadderPing".getBytes()))
				} catch {
					case ioe: IOException =>
						log.warning("problems sending ping", ioe)
				}
			}else{
				self ! PoisonPill
			}
		}
		
		{
			case InitWsPage(page) => 
				import org.ladderframework.json._
				session.addMessageHandler(new MessageHandler.Whole[String](){
					override def onMessage(msg: String) = {
						msg.toJson match {
							case Success(json:JObject) =>
								log.debug("new message: " + json)
								page ! WsCallMessage(json)
							case Success(s) =>
								log.warning("Not a JObject: " + s)
							case Failure(f) => 
								log.warning("Unable to parse message", f)
								
						}
					}
				})
				session.addMessageHandler(new MessageHandler.Whole[PongMessage](){
					override def onMessage(msg: PongMessage) = {
						page ! Ping
					}
				})
				
				context.system.scheduler.schedule(10 millis, 30 seconds, self, WsActor.Ping) //PING PONG???
				
				context.become({
					case Ping => 
						ping()
					case Nil =>
					case msg: PushMessage =>
						send(List(msg))
					case msgs: List[_] =>
						val pushMsgs: List[PushMessage] = msgs.collect({ case msg: PushMessage => msg })
						send(pushMsgs)
				})
		}
	}
}

