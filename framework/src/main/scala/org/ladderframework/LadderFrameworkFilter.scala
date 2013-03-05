package org.ladderframework

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import org.ladderframework.logging.Loggable
import akka.actor.actorRef2Scala
import akka.actor.ActorSystem
import akka.actor.Props
import akka.routing.RoundRobinRouter
import javax.servlet.annotation.MultipartConfig
import javax.servlet.annotation.WebFilter
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import javax.servlet.AsyncEvent
import javax.servlet.AsyncListener
import javax.servlet.Filter
import javax.servlet.FilterChain
import javax.servlet.FilterConfig
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import com.typesafe.config.ConfigFactory
import bootstrap.LadderBoot
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConverters.mapAsScalaMapConverter
import javax.servlet.http.HttpSessionListener
import javax.servlet.http.HttpSessionEvent

@WebFilter(urlPatterns = Array("/*"), asyncSupported = true)
@MultipartConfig(location = "/tmp", fileSizeThreshold = 1048576, maxFileSize = 52428800, maxRequestSize = 52428800)
class LadderFrameworkFilter extends Filter with Loggable {

	import bootstrap.LadderBoot.system
	
  // create the result listener, which will print the result and shutdown the system
  val master = system.actorOf(Props[Master], name = "master")
  def requestHandler = system.actorFor(master.path / "requestHandler")
	var config: FilterConfig = _
	
	lazy val asyncListener = new AsyncListener{
		def onStartAsync(evt: AsyncEvent) { debug("startAsync: " + evt.getSuppliedRequest().getLocalName())}
		def onError(evt: AsyncEvent) {
			warn("Error" + evt.getSuppliedRequest().getLocalName())
			evt.getAsyncContext().complete()
		}
		def onTimeout(evt: AsyncEvent){
			info("timeout: " + evt.getSuppliedRequest().asInstanceOf[HttpServletRequest].getRequestURI())
			evt.getAsyncContext().complete()
		}
		def onComplete(evt: AsyncEvent){debug("complete: " + evt.getSuppliedRequest().asInstanceOf[HttpServletRequest].getRequestURI())}
	}

	def init(config: FilterConfig) {
		debug("init")
		this.config = config
		val cxt = config.getServletContext()
		cxt.addListener(new HttpSessionListener{
			def sessionCreated(sessionEvent: HttpSessionEvent) {
				val sessionId = sessionEvent.getSession.getId
				debug("Create session: " + sessionId)
				master ! CreateSession(sessionId)
			} 
			def sessionDestroyed(sessionEvent: HttpSessionEvent) {
				val sessionId = sessionEvent.getSession.getId
				debug("Remove session: " + sessionId)
				master ! RemoveSession(sessionId)
			} 
		})
		LadderBoot.mimeTypeImpl = cxt.getMimeType
		LadderBoot.resourceAsStreamImpl = cxt.getResourceAsStream
		LadderBoot.resourceImpl = (s:String) => if(s.startsWith("/")) cxt.getResource(s) else cxt.getResource("/" + s)
		LadderBoot.contextPathImpl = cxt.getContextPath
		
	}
	
	def doFilter(servletRequest: ServletRequest, servletResponse: ServletResponse, chain: FilterChain) {
		
		(servletRequest, servletResponse) match {
			case (httpServletRequest: HttpServletRequest, httpServletResponse: HttpServletResponse) =>
				debug("New request to: " + httpServletRequest.getServletPath())
				
				val parts = if(Option(httpServletRequest.getContentType).map(_.startsWith("multipart/form-data")).getOrElse(false)) 
					httpServletRequest.getParts.toList else Nil
				
				val request = HttpRequest(method = Method(httpServletRequest.getMethod),
						sessionID = httpServletRequest.getSession().getId(),
						path = httpServletRequest.getServletPath.split("/").filterNot(_.isEmpty).toList,
						parameters = httpServletRequest.getParameterMap.asScala.toMap, 
						parts = parts) //TODO S wrap Part in something appropriate
				val asyncContext = httpServletRequest.startAsync
				asyncContext.addListener(asyncListener)
				val response = asyncContext.getResponse match {
						case http:HttpServletResponse => http
						case _ => 
							warn("wrong httpServletResponse")
							httpServletResponse
				}
				requestHandler ! HttpInteraction(asyncContext, request, response)
			case _ =>
				chain.doFilter(servletRequest, servletResponse);
		}
	}

	def destroy() {
		config = null
		system.shutdown()
	}
	
}