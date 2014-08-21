package org.ladderframework.mock

import java.util.concurrent.CountDownLatch
import javax.servlet.AsyncContext
import javax.servlet.AsyncEvent
import javax.servlet.AsyncListener
import javax.servlet.ServletContext
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import org.ladderframework.AsyncRequestHandler
import org.ladderframework.NotImplemented

class AsyncRequestHandlerMock(httpResponseOutput: HttpResponseOutputMock, countDown:Int = 1) extends AsyncRequestHandler {

	var timeout = 3000L
	val latch = new CountDownLatch(countDown)
	
	var errorListeners:List[() => Unit] = Nil
	var timeoutListeners:List[() => Unit] = Nil
	
	def sendTimeout() = {
		timeoutListeners.foreach(_.apply())
	}
	
	def sendError() = {
		errorListeners.foreach(_.apply())
	}
	
	def addListeners(onComplete: () => Unit, onError: () => Unit, onStart: () => Unit, onTimeout: () => Unit) = {
		errorListeners = onError :: errorListeners
		timeoutListeners = onTimeout :: timeoutListeners
	}
	def complete() = {
		if(httpResponseOutput.status == NotImplemented) throw new IllegalStateException("status not set")
		latch.countDown()
	}
	
}