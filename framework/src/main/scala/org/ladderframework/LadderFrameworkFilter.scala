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
import org.eclipse.jetty.server.handler.ContextHandler

class LadderHandler(contextHandler: ContextHandler, boot: DefaultBoot) extends AbstractHandler with Loggable {

	import boot.system
	
  // create the result listener, which will print the result and shutdown the system

  var initialized = false
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
		
		if(!initialized){
			boot.mimeTypeImpl = contextHandler.getMimeTypes.getMimeMap.get
//			def getResource(s: String) = if(s.startsWith("/")) contextHandler.getResource(s) else contextHandler.getResource("/" + s)
//			boot.resourceAsStreamImpl = s => getResource(s).getInputStream
//			boot.resourceImpl = (s:String) => getResource(s).getURL
			boot.contextPathImpl = contextHandler.getContextPath()
			debug(s"boot.contextPathImpl: ${boot.contextPathImpl}")
			initialized = true;
		}
		
		val asyncContext = httpServletRequest.startAsync
		asyncContext.addListener(asyncListener)
		val asyncResponse = asyncContext.getResponse match {
				case http:HttpServletResponse => http
				case _ => 
					warn("wrong httpServletResponse")
					httpServletResponse
		}
		system.actorOf(RequestHandler.create(boot, () => baseRequest.setHandled(true), httpServletRequest, asyncResponse))
	}

	override def destroy() = {
		boot.onShutdown()
		system.shutdown()
	}
	
}