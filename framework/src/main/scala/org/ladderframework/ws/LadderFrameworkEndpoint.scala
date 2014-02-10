package org.ladderframework.ws

import javax.websocket.server.ServerEndpoint
import javax.websocket.OnMessage
import javax.websocket.Endpoint
import javax.websocket.Session
import javax.websocket.EndpointConfig
import javax.websocket.CloseReason


@ServerEndpoint("/ws")
class LadderFrameworkEndpoint extends Endpoint {
	override def onOpen(session:Session, config:EndpointConfig ) {
		???
	}
	override def onClose(session:Session, closeReason:CloseReason) {
		???
	}
	override def onError (session:Session, throwable:Throwable) {
		???
	}
}