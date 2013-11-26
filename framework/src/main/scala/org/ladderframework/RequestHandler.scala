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
import bootstrap.LadderBoot
import org.ladderframework.js.JsCmd
import scala.concurrent.Promise

case class HttpInteraction(asyncHandler: AsyncRequestHandler, req:HttpRequest, res: HttpResponseOutput)
case class RenderInital(res: HttpResponseOutput, asyncHandler:AsyncRequestHandler)
case class CreateSession(sessionId: String)
case class RemoveSession(sessionId: String)
case class AddResponse(path: List[String], uuid:String, response: HttpResponse){}
case class PushMessage(id: String = Utils.uuid, message: String){
	lazy val asJson = {"""{"id":"""" + id + """", "message":"""" + message.replace("\"", "\\\"") + """"}"""}
}


class RequestHandler extends Actor with ActorLogging {
	
	override def receive = {
		case hi : HttpInteraction =>
			log.debug("receive - HttpInteraction: " + hi)
			val sessionID = hi.req.sessionID
			// If request to existing find actor. Find and send
			val session = context.system.actorSelection("user/" + sessionID)
			session ! hi
	}
	
}

object SessionActor{
	def apply(sessionID:String): Props = Props(new SessionActor(sessionID))
}

class SessionActor(sessionID:String) extends Actor with ActorLogging{
	def receive = {
		case hi : HttpInteraction =>
			val path = hi.req.path
			// If request to existing find actor. Find and send
			path match {
				case ("ajax" | "post" | "pull") :: id :: _ => //Match mer
					context.actorSelection(id) ! hi
				case _ if hi.req.parameters.headOption.flatMap(_._2.headOption).exists( _ == "redirect") =>
					log.debug("redirect")
					context.children.map(_.path).foreach(p => log.debug(p.toString))
					hi.req.parameters.headOption.map(_._1) match {
						case Some(id) => 
							log.debug("redirect:id = " + id)
							log.debug("child: " + context.child(id))
							context.child(id).getOrElse{
								val uuid:String = UUID.randomUUID.toString
								context.actorOf(Props(new InitalResponseContainer(context.self, hi.req, uuid)), name = uuid)
							} ! RenderInital(hi.res, hi.asyncHandler)
						case _ => 
							val uuid:String = UUID.randomUUID.toString
							val resonseContainerRef = context.actorOf(Props(new InitalResponseContainer(context.self, hi.req, uuid)), name = uuid)
							resonseContainerRef ! RenderInital(hi.res, hi.asyncHandler)
					} 
					
				case _ => 
					val uuid:String = UUID.randomUUID.toString
					val resonseContainerRef = context.actorOf(Props(new InitalResponseContainer(context.self, hi.req, uuid)), name = uuid)
					resonseContainerRef ! RenderInital(hi.res, hi.asyncHandler)
			}
		case AddResponse(path, uuid, response) =>
			context.actorOf(Props(new RedirectResponseContainer(context.self, Promise().success(response), uuid)), name = uuid)
	}
}



class RedirectResponseContainer(
		val session:ActorRef, 
		val httpResponse:Promise[HttpResponse], 
		val uuid:String) extends ResponseContainer

class InitalResponseContainer(
		val session:ActorRef, 
		val initalRequest:HttpRequest, 
		val uuid:String) extends ResponseContainer{
	
	val httpResponse:Promise[HttpResponse] = Promise().completeWith((Future(LadderBoot.site).map(_ orElse notFound)).flatMap(_.apply(initalRequest)))
	
} 

case object Tick

trait ResponseContainer extends Actor with ActorLogging{
	
	def httpResponse: Promise[HttpResponse]
	val uuid: String
	def session: ActorRef
	private var messages: List[PushMessage] = Nil
	private var lastAccess = System.currentTimeMillis	
	
	var isTerminated = false
	
	def updateLastAccess(){
		context.system.scheduler.scheduleOnce(LadderBoot.timeToLivePage millis, context.self, Tick)
		lastAccess = System.currentTimeMillis
	}
	
	override def preStart() {
		updateLastAccess()
		isTerminated = false
	}
	
	override def postStop() {
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
			Future(LadderBoot.notFound)
	}
	
	def errorHandle: PartialFunction[(Status, Option[Throwable]), HttpResponse] = {
		case (s, ot) => ErrorResponse(s, ot)
	}
	
	def update(message: JsCmd): Try[Unit] = {
		if(!isTerminated){
			Try{(self ! PushMessage(message = message.toCmd))}
		}else{
			Failure(new IllegalStateException("page is closed"))
		}
	}
	
	implicit val statefulContext = new Context(uuid, addResponse, update)
	
	def receive = {
		case Tick => 
			log.debug("Tick")
			if(LadderBoot.timeToLivePage + System.currentTimeMillis > lastAccess) {
				self ! PoisonPill
			}
		case RenderInital(res, asyncContext) =>
			log.debug("RenderInital")
			updateLastAccess()
			
			def complete() = {
				log.debug("Completed")
				asyncContext.complete()
	    	httpResponse.future.map(_ match {
					case _ : Stateful => 
					case _ => self ! PoisonPill
				})
			}
			val future = httpResponse.future.flatMap(_.applyToHttpServletResponse(res))
			future.onComplete{
				case Failure(t) =>
					log.error(t, "Problems handling request: " + httpResponse)
					(LadderBoot.errorHandle orElse errorHandle)((InternalServerError, Option(t))).applyToHttpServletResponse(res).onComplete{
							case _ => complete()
					}
				case Success(status:Status) => 
					log.debug("Status: " + status + "; httpResponse: " + Await.ready(httpResponse.future, 10 seconds)); 
					complete()
			}
    	
		case newMessage : PushMessage =>
			log.debug("newMessage:" + newMessage)
			updateLastAccess()
			messages = messages :+ newMessage
			log.debug("messages:" + messages)
			context.children.foreach(_ ! newMessage)
		case HttpInteraction(asyncContext, request @ HttpRequest(_, "pull" :: `uuid` :: _), res) => 
			log.debug("pull")
			updateLastAccess()
			request.parameters.get("lastId").flatMap(_.headOption).foreach(id => { 
					messages = messages.reverse.takeWhile(_.id != id).reverse
			})
			context.actorOf(Props(new PullActor(asyncContext, res))) ! messages
			
		case HttpInteraction(asyncContext, req, res) =>
			updateLastAccess()
			val response: Future[HttpResponse] = (statefulContext.submitCallback orElse 
					statefulContext.ajaxSubmitCallback orElse
					statefulContext.ajaxHandlerCallback orElse
					statefulContext.ajaxCallback orElse notFound).apply(req)
			response.onComplete{
					case Success(http) => 
						log.debug("completing - success, http: " + http)
						http.applyToHttpServletResponse(res).onComplete {
							case Success(status) => 
								asyncContext.complete()
								log.debug("completed - success, status: " + status)
							case Failure(throwable) =>
								log.error(throwable, "problem completing")
								asyncContext.complete()
						}
						
					case Failure(fail) => {
						log.error("complete - fail: " + fail)
						ErrorResponse(InternalServerError, Some(fail)).applyToHttpServletResponse(res).onComplete {
							case Success(status) => 
								asyncContext.complete()
								log.debug("completed - fail, status: " + status)
							case Failure(throwable) =>
								log.error("problem completing fail result", throwable)
								asyncContext.complete()
						}
					}
			}
			
	}
	
}

class PullActor(asyncHandler: AsyncRequestHandler, res:HttpResponseOutput) extends Actor with ActorLogging {
	
	object TimeToClose
	
	val onComplete = () => {
		context.self ! PoisonPill
	}
	val onError = () => {
		context.self ! PoisonPill
	}
	val onStart = () => {}
	val onTimeout = () => {
		context.self ! PoisonPill
	}
	
	asyncHandler.addListeners(onComplete, onError, onStart, onTimeout)
	
	context.system.scheduler.scheduleOnce(24000 millis, context.self, TimeToClose)
	
	def receive = {
		case TimeToClose => 
			send(Nil)
		case Nil =>
		case msg : PushMessage =>
			send(List(msg))
		case msgs : List[_] => 
			val pushMsgs: List[PushMessage] = msgs.collect({case msg:PushMessage => msg})
			send(pushMsgs)
	}
	
	def send(msgs:List[PushMessage]) {
		val response = PullResponse(msgs)
		implicit val context:Context = null
		response.applyToHttpServletResponse(res).onComplete{
			case Success(status) => 
				asyncHandler.complete()
				log.debug("pull completed - success, status: " + status)
			case Failure(throwable) =>
				log.warning("pull problem completing fail result", throwable)
				asyncHandler.complete()
		}
	}
}

