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

@WebFilter(urlPatterns = Array("/*"), asyncSupported = true)
@MultipartConfig(location = "/tmp", fileSizeThreshold = 1048576, maxFileSize = 52428800, maxRequestSize = 52428800)
class LadderFrameworkFilter extends Filter with Loggable {

	val akkaConfig = ConfigFactory.load()
	val system = ActorSystem("WebSystem", akkaConfig)
 
    // create the result listener, which will print the result and shutdown the system
  val master = system.actorOf(Props[Master].withRouter(RoundRobinRouter(10)), name = "master")
  val requestHandler = system.actorFor(master.path / "requestHandler")
	var config: FilterConfig = _
	
	lazy val asyncListener = new AsyncListener{
		def onStartAsync(evt: AsyncEvent) { debug("startAsync: " + evt.getSuppliedRequest().getLocalName())}
		def onError(evt: AsyncEvent) {warn("Error" + evt.getSuppliedRequest().getLocalName())}
		def onTimeout(evt: AsyncEvent){info("timeout: " + evt.getSuppliedRequest().asInstanceOf[HttpServletRequest].getRequestURI())}
		def onComplete(evt: AsyncEvent){debug("complete: " + evt.getSuppliedRequest().asInstanceOf[HttpServletRequest].getRequestURI())}
	}

	def init(config: FilterConfig) {
		debug("init")
		this.config = config
		val cxt = config.getServletContext()
		LadderBoot.mimeType = cxt.getMimeType
		LadderBoot.resourceAsStream = cxt.getResourceAsStream
		LadderBoot.resource = cxt.getResource
		LadderBoot.contextPath = cxt.getContextPath
		
	}
	
	def doFilter(servletRequest: ServletRequest, servletResponse: ServletResponse, chain: FilterChain) {
		
		(servletRequest, servletResponse) match {
			case (httpServletRequest: HttpServletRequest, httpServletResponse: HttpServletResponse) =>
				debug("New request to: " + httpServletRequest.getServletPath())
				val request = HttpRequest(method = Method(httpServletRequest.getMethod),
						sessionID = httpServletRequest.getSession().getId(),
						path = httpServletRequest.getServletPath.split("/").filterNot(_.isEmpty).toList,
						parameters = httpServletRequest.getParameterMap.asScala.toMap, 
						parts = httpServletRequest.getParts.toList) //TODO S wrap Part in something appropriate
				val asyncContext = httpServletRequest.startAsync
				asyncContext.addListener(asyncListener)
				requestHandler ! HttpInteraction(asyncContext, request, httpServletResponse)
			case _ =>
				chain.doFilter(servletRequest, servletResponse);
		}
	}

	def destroy() {
		config = null
		system.shutdown()
	}
	
}