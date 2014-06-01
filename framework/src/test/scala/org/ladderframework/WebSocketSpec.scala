package org.ladderframework

import java.util.concurrent.TimeUnit
import org.ladderframework.mock.EndpointConfigMock
import org.ladderframework.mock.WsSessionMock
import org.ladderframework.ws.LadderFrameworkEndpoint
import org.scalatest.BeforeAndAfterAll
import org.scalatest.GivenWhenThen
import org.scalatest.WordSpecLike
import akka.actor.ActorSystem
import akka.testkit.TestKit
import javax.websocket.server.ServerEndpoint
import java.util.UUID
import akka.actor.Props
import akka.testkit.TestActorRef
import akka.actor.Actor
import akka.actor.ActorRef
import bootstrap.LadderBoot
import akka.testkit.TestProbe
import javax.websocket.MessageHandler
import javax.websocket.PongMessage
import org.ladderframework.json._

class WebSocketSpec (system: ActorSystem) extends TestKit(system) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll{
	
	implicit val webSystem = system
	
	def this() = this(ActorSystem("WebSystem"))

	"The web socket actor" when {
		"handle polling" should {
			"handle pushing message" in {
				val sessionId = Utils.uuid
				val pageId = Utils.uuid
				val pageProbe = TestProbe()
				val uuid = Utils.uuid
				val sessionActor = system.actorOf(Props(new Actor{
					override def receive = {
						case ws:WsConnect =>
							sender ! InitWsPage(pageProbe.ref)
							sender ! List(PushMessage(message = uuid)) 
					}
				}), name = sessionId)
				
				val wsSession = new WsSessionMock("somePage", "")
				val config = new EndpointConfigMock(sessionId, webSystem)

				val endpoint = new LadderFrameworkEndpoint
				endpoint.onOpen(wsSession, config)
				wsSession.latch.await(3, TimeUnit.SECONDS)
				assert(wsSession.messages.size === 1)
				val text = wsSession.messages.head
				assert(text !== """{"messages":[]}""")
				assert(text.contains(uuid))
			}
			"handle sending messages" in {
				val sessionId = Utils.uuid
				val pageId = Utils.uuid
				val pageProbe = TestProbe()
				val uuid = Utils.uuid
				val sessionActor = system.actorOf(Props(new Actor{
					override def receive = {
						case ws:WsConnect =>
							sender ! InitWsPage(pageProbe.ref)
							sender ! Nil
					}
				}), name = sessionId)
				
				val wsSession = new WsSessionMock("somePage", "")
				val config = new EndpointConfigMock(sessionId, webSystem)

				val endpoint = new LadderFrameworkEndpoint
				endpoint.onOpen(wsSession, config)
				awaitCond(wsSession.messageHandlers.size == 2)
				val handler = wsSession.messageHandlers.tail.head.asInstanceOf[MessageHandler.Whole[String]]
				val obj = JObject("call" -> "someone", "or" -> "someone else")
				handler.onMessage(obj.nospace)
				pageProbe.expectMsg(WsCallMessage(obj))
			}
			"handle sending ping pong messages" in {
				val sessionId = Utils.uuid
				val pageId = Utils.uuid
				val pageProbe = TestProbe()
				val uuid = Utils.uuid
				val sessionActor = system.actorOf(Props(new Actor{
					override def receive = {
					case ws:WsConnect =>
					sender ! InitWsPage(pageProbe.ref)
					sender ! Nil
					sender ! Ping
					}
				}), name = sessionId)
						
				val wsSession = new WsSessionMock("somePage", "")
				val config = new EndpointConfigMock(sessionId, webSystem)
				
				val endpoint = new LadderFrameworkEndpoint
				endpoint.onOpen(wsSession, config)
				awaitCond(wsSession.messageHandlers.size == 2)
				val handler = wsSession.messageHandlers.head.asInstanceOf[MessageHandler.Whole[PongMessage]] 
				awaitCond(wsSession.gotPing)
				handler.onMessage(new PongMessage{
					override def getApplicationData = ???
				})
				pageProbe.expectMsg(Ping)
			}
		}
	}
	
	override def afterAll {
    system.shutdown()
  }
}