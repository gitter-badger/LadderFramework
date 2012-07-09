package org.ladderframework.mock

import javax.servlet.AsyncContext
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.AsyncListener
import javax.servlet.ServletContext
import java.util.concurrent.CountDownLatch
import javax.servlet.AsyncEvent

class AsyncContextMock(countDown:Int = 1) extends AsyncContext {

	var timeout = 3000L
	val latch = new CountDownLatch(countDown)
	
	private var listeners:List[AsyncListener] = Nil
	
	def sendTimeout{
		val event = new AsyncEvent(this) 
		listeners.foreach(_.onTimeout(event))
	}
	
	def sendError{
		val event = new AsyncEvent(this) 
		listeners.foreach(_.onError(event))
	}
	
	def getTimeout(): Long = timeout
	def setTimeout(timeout: Long) {this.timeout = timeout}
	def createListener[T <: AsyncListener](x$1: Class[T]): T = null.asInstanceOf[T]
	def addListener(listener: AsyncListener, req: ServletRequest, res: ServletResponse) {
		addListener(listener)
	}
	def addListener(listener: AsyncListener) {
		listeners = listeners :+ listener
	}
	def start(runnable: java.lang.Runnable) {}
	def complete() {
		latch.countDown()
	}
	def dispatch(context: ServletContext, x$2: java.lang.String) {}
	def dispatch(x$1: java.lang.String) {}
	def dispatch() {}
	def hasOriginalRequestAndResponse(): Boolean = false
	def getResponse(): ServletResponse = null
	def getRequest(): ServletRequest = null
	
}