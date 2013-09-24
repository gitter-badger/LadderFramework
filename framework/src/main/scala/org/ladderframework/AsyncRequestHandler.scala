package org.ladderframework

import javax.servlet.AsyncContext
import javax.servlet.AsyncListener
import javax.servlet.AsyncEvent

trait AsyncRequestHandler {
	def addListeners(onComplete: () => Unit, onError: () => Unit, onStart: () => Unit, onTimeout: () => Unit): Unit
	def complete(): Unit
}

class AsyncServletContext(asyncContext: AsyncContext) extends AsyncRequestHandler{
	
	def addListeners(onCompleteCallback: () => Unit, onErrorCallback: () => Unit, onStartCallback: () => Unit, onTimeoutCallback: () => Unit){
		asyncContext.addListener(new AsyncListener(){
			def onComplete(event: AsyncEvent) {
				onCompleteCallback()
			}
			def onError(event: AsyncEvent){
				onErrorCallback()
			}
			def onStartAsync(event: AsyncEvent){
				onStartCallback()
			}
			def onTimeout(event: AsyncEvent){
				onTimeoutCallback()
			}
		})
		
	}
	def complete(){
		asyncContext.complete()
	}
	
}