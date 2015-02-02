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
import org.eclipse.jetty.continuation.ContinuationSupport

class LadderHandler(contextHandler: ContextHandler, boot: DefaultBoot) extends AbstractHandler with Loggable {

	import boot.system

	// create the result listener, which will print the result and shutdown the system

	var initialized = false
	def sendMyTimeoutResponse(response: HttpServletResponse): Unit = {
		response.setStatus(Status.RequestTimeOut.code)
		val os = response.getOutputStream
		os.println("The request timed out")
		os.close()
	}

	override def handle(target: String,
			baseRequest: Request,
			httpServletRequest: HttpServletRequest,
			httpServletResponse: HttpServletResponse): Unit = {
		debug("New request to: " + httpServletRequest.getPathInfo())

		if (!initialized) {
			boot.mimeTypeImpl = contextHandler.getMimeTypes.getMimeMap.get
			//			def getResource(s: String) = if(s.startsWith("/")) contextHandler.getResource(s) else contextHandler.getResource("/" + s)
			//			boot.resourceAsStreamImpl = s => getResource(s).getInputStream
			//			boot.resourceImpl = (s:String) => getResource(s).getURL
			boot.contextPathImpl = contextHandler.getContextPath()
			debug(s"boot.contextPathImpl: ${boot.contextPathImpl}")
			initialized = true;
		}

		val continuation = ContinuationSupport.getContinuation(httpServletRequest)
		if (continuation.isExpired()) {
			sendMyTimeoutResponse(httpServletResponse)
		} else {
			continuation.suspend(httpServletResponse); // response may be wrapped.
			system.actorOf(RequestHandler.create(boot, () => continuation.complete(), httpServletRequest, httpServletResponse))
		}

	}

	override def destroy() = {
		boot.onShutdown()
		system.shutdown()
	}

}