package org.ladderframework

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import java.util.concurrent.TimeUnit._
import java.util.UUID
import akka.actor._
import akka.routing.RoundRobinRouter
import org.ladderframework.js.JsCmd
import scala.concurrent.Promise
import java.io.IOException
import org.ladderframework.json._
import scala.util.Failure

case class HttpInteraction(req: HttpRequest, res: Promise[HttpResponseOutput])
case class RenderInital(res: Promise[HttpResponseOutput])
case class CreateSession(sessionId: String)
case class RemoveSession(sessionId: String)
case class AddResponse(path: List[String], uuid: String, response: HttpResponse) {}
case class PushMessage(id: String = Utils.uuid, message: String) {
	lazy val asJson = { """{"id":"""" + id + """", "message":"""" + message.replace("\"", "\\\"") + """"}""" }
}

//case class WsConnect(sessionID: String, page: String, lastId: String, actor: ActorRef)
//case class InitWsConnection(session: WsSession, httpSessionID: String)
//case class InitWsPage(page: ActorRef)
//case class WsCallMessage(msg: JObject)
case object Ping

class RequestHandler extends Actor with ActorLogging {

	override def receive = {
		case hi: HttpInteraction =>
			log.debug("receive - HttpInteraction: " + hi)
			val sessionID = hi.req.sessionID
			// If request to existing find actor. Find and send
			val session = context.system.actorSelection("user/" + sessionID.value)
			session ! hi
//		case ws: WsConnect =>
//			log.debug("receive - WsInteraction: " + ws)
//			val sessionID = ws.sessionID
//			val session = context.system.actorSelection("user/" + sessionID.value)
//			session ! ws
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
			send(Nil)
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

