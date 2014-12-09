//package org.ladderframework.mock
//
//import java.util.UUID
//import java.util.concurrent.CountDownLatch
//
//import javax.websocket.CloseReason
//import javax.websocket.MessageHandler
//import javax.websocket.RemoteEndpoint
//import javax.websocket.Session
//import javax.websocket.WebSocketContainer
//
//class WsSessionMock(page: String, lastId: String, countDown: Int = 1) extends Session{
//
//	val latch = new CountDownLatch(countDown)
//	var messageHandlers: List[MessageHandler] = Nil
//	
//	var messages: List[String] = Nil
//	
//	var gotPing = false
//	
//  override def getContainer():WebSocketContainer = ???
//  
//  override def addMessageHandler(mh: MessageHandler) {
//		messageHandlers +:= mh
//	}
//  
//  override def getMessageHandlers(): java.util.Set[MessageHandler] = ???
//  
//  override def removeMessageHandler(mh: MessageHandler){???}
//  
//  override def getProtocolVersion(): String = ???
//  
//  override def getNegotiatedSubprotocol() = ???
//  
//  override def getNegotiatedExtensions() = ???
//  
//  override def isSecure() = ???
//
//  override def isOpen() = true
//  
//  override def getMaxIdleTimeout() = ???
//  
//  override def setMaxIdleTimeout(newTime: Long) = ???
//  
//  override def setMaxBinaryMessageBufferSize(arg: Int){ ??? }
//  
//  override def getMaxBinaryMessageBufferSize() = ???
//  
//  override def setMaxTextMessageBufferSize(arg: Int) = ???
//  
//  override def getMaxTextMessageBufferSize() = ???
//  
//  override def getAsyncRemote = ???
//  
//  override def getBasicRemote() = new RemoteEndpoint.Basic{
//	  override def sendText(msg: String){
//	  	messages +:= msg
//	  }
//	  
//	  override def sendBinary(arg: java.nio.ByteBuffer) = ???
//	  override def sendText(arg0: String, arg1: Boolean) = ???
//	  override def sendBinary(arg: java.nio.ByteBuffer, arg1: Boolean) = ???
//	  override def getSendStream() = ???
//	  override def getSendWriter() = ???
//	  override def sendObject(arg0: AnyRef) = ???
//	  override def setBatchingAllowed(arg0: Boolean) = ???
//	  override def getBatchingAllowed() = ???
//	  override def flushBatch() = ???
//	  override def sendPing(arg0: java.nio.ByteBuffer) = {gotPing = true}
//	  override def sendPong(arg0: java.nio.ByteBuffer) = ???
//  }
//  
//  override val getId = UUID.randomUUID.toString 
//  
//  override def close() = ???
//  
//  override def close(arg: CloseReason) = ???
//  
//  override def getRequestURI() = ???
//  
//  override def getRequestParameterMap() = ???
//  
//  override def getQueryString() = ???
//  
//  override val getPathParameters = {
//  	scala.collection.JavaConversions.mapAsJavaMap(Map("lastId" -> lastId, "page" -> page))
//  }
//  
//  override def getUserProperties = ???
//  
//  override def getUserPrincipal() = ???
//  
//  override def getOpenSessions() = ???
//	
//}