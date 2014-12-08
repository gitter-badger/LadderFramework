//package org.ladderframework
//package ws
//
//import javax.websocket.server.ServerEndpoint
//import javax.websocket.OnMessage
//import javax.websocket.Endpoint
//import javax.websocket.Session
//import javax.websocket.EndpointConfig
//import javax.websocket.CloseReason
//import javax.websocket.server.ServerEndpointConfig
//import javax.websocket.HandshakeResponse
//import javax.servlet.http.HttpSession
//import javax.websocket.server.HandshakeRequest
//import scala.concurrent.Promise
//import org.ladderframework.logging.Loggable
//import akka.actor.PoisonPill
//import akka.actor.ActorRef
//import akka.actor.ActorSystem
//
//
//@ServerEndpoint("/ws/{page}/{lastId}")
//class LadderFrameworkEndpoint extends Endpoint with Loggable {
//	
//	var actor = Promise[ActorRef]()
//	
//	override def onOpen(session:Session, config:EndpointConfig ) {
//		debug("open session: " + session.getId())
//    val httpSession = config.getUserProperties().get(GetHttpSessionConfigurator.sessionKey).asInstanceOf[String]
//		val actorSystem = config.getUserProperties().get(GetHttpSessionConfigurator.actorSystem).asInstanceOf[ActorSystem]
//		val ac = actorSystem.actorOf(WsActor.props)
//		ac ! InitWsConnection(session, httpSession)
//		actor.success(ac)
//	}
//	
//	private def killActor(){
//		actor.future.value.foreach(_.foreach(_ ! PoisonPill))
//	}
//	
//	override def onClose(session:Session, closeReason:CloseReason) {
//		debug("close session: " + session.getId())
//		killActor()
//	}
//	override def onError (session:Session, throwable:Throwable) {
//		warn("error on session: " + session.getId(), throwable)
//		session.close()
//		killActor()
//	}
//}
//
//object GetHttpSessionConfigurator{
//	final val sessionKey = "HttpSession"
//	final val actorSystem = "WebActorSystem"
//}
//
//class GetHttpSessionConfigurator extends ServerEndpointConfig.Configurator{
//   
//	override def modifyHandshake(config:ServerEndpointConfig, 
//                                request:HandshakeRequest, 
//                                response:HandshakeResponse ){
//    val httpSessionId = request.getHttpSession.asInstanceOf[HttpSession].getId()
//    config.getUserProperties().put(GetHttpSessionConfigurator.sessionKey, httpSessionId)
//    config.getUserProperties().put(GetHttpSessionConfigurator.actorSystem, LadderBoot.system)
//    
//  }
//}