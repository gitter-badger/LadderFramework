package org.ladderframework.mock

import javax.websocket.EndpointConfig
import org.ladderframework.ws.GetHttpSessionConfigurator
import java.util.UUID
import akka.actor.ActorSystem

class EndpointConfigMock(sessionId: String, actorSystem: ActorSystem) extends EndpointConfig {

	override def getEncoders() = ???
  
  override def getDecoders() = ???
  
  override val getUserProperties = {
  	scala.collection.JavaConversions.mapAsJavaMap(Map[String, AnyRef](
  			GetHttpSessionConfigurator.sessionKey -> sessionId,
  			GetHttpSessionConfigurator.actorSystem -> actorSystem
  	))
  }
	
}