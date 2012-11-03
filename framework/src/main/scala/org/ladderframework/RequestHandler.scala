package org.ladderframework

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit._
import java.util.UUID
import akka.actor.actorRef2Scala
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.actor.Props
import akka.routing.RoundRobinRouter
import bootstrap.LadderBoot
import javax.servlet.http.HttpServletResponse
import javax.servlet.AsyncContext
import javax.servlet.AsyncListener
import javax.servlet.AsyncEvent
import org.ladderframework.js.JsCmd

case class HttpInteraction(asyncContext: AsyncContext, req:HttpRequest, res: HttpServletResponse)
case class RenderInital(res: HttpServletResponse, asyncContext:AsyncContext)
case class CreateSession(sessionId: String)
case class RemoveSession(sessionId: String)
case class AddResponse(path: List[String], uuid:String, response: HttpResponse)
case class PushMessage(id: String = Utils.uuid, message: String){
	lazy val asJson = """{"id":"""" + id + """", "message":"""" + message.replace("\"", "\\\"") + """"}"""
}

class Master extends Actor with ActorLogging {
 
	val requestHandler = context.actorOf(Props[RequestHandler].withRouter(RoundRobinRouter(10)), name = "requestHandler")
	
	override def receive = {
		case hi : HttpInteraction => 
			log.debug("receive - HttpInteraction: " + hi)
			requestHandler ! hi 
		case CreateSession(sessionID) => 
			log.debug("receive - CreateSession: " + sessionID)
			context.actorOf(Props(new SessionActor(sessionID)), name = sessionID)
		case RemoveSession(sessionID) =>
			log.debug("receive - RemoveSession: " + sessionID)
			val session:ActorRef = context.actorFor(sessionID:: Nil)
			session ! PoisonPill
	}
	
}

class RequestHandler extends Actor with ActorLogging {
	
	override def receive = {
		case hi : HttpInteraction =>
			log.debug("receive - HttpInteraction: " + hi)
			val sessionID = hi.req.sessionID
			// If request to existing find actor. Find and send
			val session:ActorRef = context.actorFor(context.system / "master" / sessionID)
			session ! hi
	}
	
}

class SessionActor(sessionID:String) extends Actor with ActorLogging{
	def receive = {
		case hi : HttpInteraction =>
			val path = hi.req.path
			// If request to existing find actor. Find and send
			path match {
				case ("ajax" | "post" | "pull") :: id :: _ => //Match mer
					context.actorFor(id) ! hi
				case _ if hi.req.parameters.headOption.flatMap(_._2.headOption).map( _ == "redirect").getOrElse(false) =>
					log.debug("redirect")
					context.children.map(_.path).foreach(p => log.debug(p.toString))
					hi.req.parameters.headOption.map(_._1) match {
						case Some(id) => 
							log.debug("redirect:id = " + id)
							log.debug("childPath: " + context.actorFor(id).path)
							context.actorFor(id) ! RenderInital(hi.res, hi.asyncContext)
						case _ => LadderBoot.notFound
					} 
					
				case _ => 
					val uuid:String = UUID.randomUUID.toString
					val resonseContainerRef = context.actorOf(Props(new InitalResponseContainer(context.self, hi.req, uuid)), name = uuid)
					resonseContainerRef ! RenderInital(hi.res, hi.asyncContext)
			}
		case AddResponse(path, uuid, response) =>
			context.actorOf(Props(new RedirectResponseContainer(context.self, response, uuid)), name = uuid)
	}
}



class RedirectResponseContainer(
		val session:ActorRef, 
		val httpResponse:HttpResponse, 
		val uuid:String) extends ResponseContainer

class InitalResponseContainer(
		val session:ActorRef, 
		val initalRequest:HttpRequest, 
		val uuid:String) extends ResponseContainer{
	
	lazy val httpResponse:HttpResponse = (LadderBoot.site orElse notFound).apply(initalRequest)
	
} 

case object Tick

trait ResponseContainer extends Actor with ActorLogging{
	
	def httpResponse: HttpResponse
	val uuid: String
	def session: ActorRef
	private var messages: List[PushMessage] = Nil
	private var lastAccess = System.currentTimeMillis	
	
	def updateLastAccess(){
		context.system.scheduler.scheduleOnce(LadderBoot.timeToLivePage millis, context.self, Tick)
		lastAccess = System.currentTimeMillis
	}
	
	override def preStart() = {
		updateLastAccess()
	}
	
	def addResponse(path: List[String], response: HttpResponse): String = {
		val uniqueID = UUID.randomUUID.toString
		session ! AddResponse(path, uniqueID, response)
		uniqueID
	}
	
	def notFound: PartialFunction[HttpRequest, HttpResponse] = {
		case _ => LadderBoot.notFound
	}
	
	def errorHandle: PartialFunction[(Status, Option[Throwable]), HttpResponse] = {
		case (s, ot) => ErrorResponse(s, ot)
	}
	
	def update(message: JsCmd) {
		context.self ! PushMessage(message = message.toCmd)
	}
	
	implicit val statefulContext = new Context(uuid, addResponse , update)
	
	def receive = {
		case Tick => 
			log.debug("Tick")
			if(LadderBoot.timeToLivePage + System.currentTimeMillis > lastAccess) {
				self ! PoisonPill
			}
		case RenderInital(res, asyncContext) =>
			log.debug("RenderInital")
			updateLastAccess()
			try{
				httpResponse.applyToHttpServletResponse(res)
			}catch{
				case t:Throwable =>
					log.error(t, "Problems handling request: " + httpResponse)
					(LadderBoot.errorHandle orElse errorHandle)((InternalServerError, Some(t))).applyToHttpServletResponse(res)
			}
    	asyncContext.complete()
    	httpResponse match {
				case _ : Stateful => 
				case _ => self ! PoisonPill
			}
		case newMessage : PushMessage =>
			log.debug("newMessage:" + newMessage)
			updateLastAccess()
			messages = messages :+ newMessage
			log.debug("messages:" + messages)
			context.children.foreach(_ ! newMessage)
		case HttpInteraction(asyncContext, req @ HttpRequest(_, _, "pull" :: `uuid` :: _, params, _), res) => 
			log.debug("pull")
			updateLastAccess()
			params.get("lastId").flatMap(_.headOption).foreach(id => { 
					messages = messages.reverse.takeWhile(_.id != id).reverse
			})
			context.actorOf(Props(new PullActor(asyncContext, res))) ! messages
			
		case HttpInteraction(asyncContext, req, res) =>
			updateLastAccess()
			val response = (statefulContext.submitCallback orElse 
					statefulContext.ajaxSubmitCallback orElse
					statefulContext.ajaxHandlerCallback orElse
					statefulContext.ajaxCallback orElse notFound).apply(req)
			response.applyToHttpServletResponse(res)
    	asyncContext.complete()
	}
	
}

class PullActor(asyncContext: AsyncContext, res:HttpServletResponse) extends Actor with ActorLogging {
	
	val asyncListener = new AsyncListener{
		def onComplete(event: AsyncEvent) {}
		def onError(event: AsyncEvent){
			context.self ! PoisonPill
		}
		def onStartAsync(event: AsyncEvent){}
		def onTimeout(event: AsyncEvent){
			context.self ! PoisonPill
		}
	}
	
	asyncContext.addListener(asyncListener)
	
	def receive = {
		case Nil =>
		case msg : PushMessage =>
			send(List(msg))
		case msgs : List[_] => 
			val pushMsgs: List[PushMessage] = msgs.collect({case msg:PushMessage => msg})
			send(pushMsgs)
	}
	
	def send(msgs:List[PushMessage]) {
		val response = PullResponse(msgs)
		response.applyToHttpServletResponse(res)(null)
		asyncContext.complete()
	}
}
