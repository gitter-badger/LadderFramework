package org.ladderframework

import org.ladderframework.logging.Loggable
import akka.actor.Props
import akka.actor.actorRef2Scala
import javax.servlet.AsyncEvent
import javax.servlet.AsyncListener
import javax.servlet.Filter
import javax.servlet.FilterChain
import javax.servlet.FilterConfig
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.annotation.MultipartConfig
import javax.servlet.annotation.WebFilter
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.HttpSessionEvent
import javax.servlet.http.HttpSessionListener
import akka.actor.PoisonPill
import akka.routing.RoundRobinPool
import org.eclipse.jetty.server.handler.AbstractHandler
import javax.servlet.ServletContext
import scala.concurrent.Promise
import org.eclipse.jetty.server.Request

class LadderHandler(boot: DefaultBoot, cxt: ServletContext) extends AbstractHandler with Loggable {

	import boot.system
	
  // create the result listener, which will print the result and shutdown the system
  val requestHandler = system.actorOf(Props[RequestHandler].withRouter(new RoundRobinPool(10)), name = "requestHandler")

  debug("init")
	cxt.addListener(new HttpSessionListener{
		def sessionCreated(sessionEvent: HttpSessionEvent) = {
			val sessionId = sessionEvent.getSession.getId
			debug("Create session: " + sessionId)
			system.actorOf(SessionActor(SessionId(sessionId), boot), name = sessionId)
		} 
		def sessionDestroyed(sessionEvent: HttpSessionEvent) = {
			val sessionId = sessionEvent.getSession.getId
			debug("Remove session: " + sessionId)
			system.actorSelection("user/" + sessionId) ! PoisonPill
		} 
		
	})
	boot.mimeTypeImpl = cxt.getMimeType
	boot.resourceAsStreamImpl = cxt.getResourceAsStream
	boot.resourceImpl = (s:String) => if(s.startsWith("/")) cxt.getResource(s) else cxt.getResource("/" + s)
	boot.contextPathImpl = cxt.getContextPath
  
	lazy val asyncListener = new AsyncListener{
		def onStartAsync(evt: AsyncEvent) = { debug("startAsync: " + evt.getSuppliedRequest().asInstanceOf[HttpServletRequest].getRequestURI())}
		def onError(evt: AsyncEvent) = {
			warn("Error" + evt.getSuppliedRequest().asInstanceOf[HttpServletRequest].getRequestURI())
			evt.getAsyncContext().complete()
			evt.getAsyncContext.getResponse.getWriter.close()
			evt.getSuppliedResponse.getWriter.close()
		}
		def onTimeout(evt: AsyncEvent) = {
			debug("timeout: " + evt.getSuppliedRequest.asInstanceOf[HttpServletRequest].getRequestURI())
			evt.getAsyncContext().complete()
			evt.getAsyncContext.getResponse.getWriter.close()
			evt.getSuppliedResponse.getWriter.close()
		}
		def onComplete(evt: AsyncEvent) = {debug("complete: " + evt.getSuppliedRequest().asInstanceOf[HttpServletRequest].getRequestURI())}
	}

	override def handle(target: String,
							baseRequest: Request,
              httpServletRequest: HttpServletRequest ,
              httpServletResponse: HttpServletResponse ): Unit = {
		debug("New request to: " + httpServletRequest.getServletPath())
		
		val request = new ServletHttpRequest(httpServletRequest)
		val asyncContext = httpServletRequest.startAsync
		asyncContext.addListener(asyncListener)
		val asyncResponse = asyncContext.getResponse match {
				case http:HttpServletResponse => http
				case _ => 
					warn("wrong httpServletResponse")
					httpServletResponse
		}
		val response = Promise[HttpResponseOutput]()
		requestHandler ! HttpInteraction(request, response)
	}

	override def destroy() = {
		boot.onShutdown()
		system.shutdown()
	}
	
}