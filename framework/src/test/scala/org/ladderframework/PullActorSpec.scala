package org.ladderframework

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestActorRef
import org.scalatest.WordSpec
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfterAll
import akka.actor.Props
import javax.servlet.AsyncContext
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import org.scalatest.WordSpecLike
import org.ladderframework.mock.HttpResponseOutputMock
import org.ladderframework.mock.AsyncRequestHandlerMock

class PullActorSpec (system: ActorSystem) extends TestKit(system) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll{
	
	implicit val webSystem = system
	
	def this() = this(ActorSystem("WebSystem"))

	"The pull actor" when {
		"handle polling" should {
			"handle pushing message" in {
				val httpResponseOutput = new HttpResponseOutputMock()
				val asyncRequestHandler = new AsyncRequestHandlerMock(httpResponseOutput)
				val pullActor = system.actorOf(Props(new PullActor(asyncRequestHandler, httpResponseOutput)))
				val uuid = Utils.uuid
				pullActor ! PushMessage(uuid, "sendMessage")
				asyncRequestHandler.latch.await(1, TimeUnit.SECONDS)
				assert(httpResponseOutput.status === OK)
				assert(httpResponseOutput.contentType === "text/json")
				val text = httpResponseOutput.text
				assert(text.contains(uuid))
				assert(text.contains("sendMessage"))
			}
			"handle pushing messages" in {
				val httpResponseOutput = new HttpResponseOutputMock()
				val asyncRequestHandler = new AsyncRequestHandlerMock(httpResponseOutput)
				val pullActor = system.actorOf(Props(new PullActor(asyncRequestHandler, httpResponseOutput)))
				val uuid = Utils.uuid
				val uuid2 = Utils.uuid
				val message1 = "sendMessage1"
				val message2 = "sendMessage2"
				pullActor ! List(PushMessage(uuid, message1), PushMessage(uuid2, message2))
				asyncRequestHandler.latch.await(1, TimeUnit.SECONDS)
				assert(httpResponseOutput.status === OK)
				assert(httpResponseOutput.contentType === "text/json")
				val text = httpResponseOutput.text
				assert(text.contains(uuid))
				assert(text.contains(message1))
				assert(text.contains(uuid2))
				assert(text.contains(message2))
			}
			
			"handle pushing no messages" in {
				val httpResponseOutput = new HttpResponseOutputMock()
				val asyncRequestHandler = new AsyncRequestHandlerMock(httpResponseOutput)
				val pullActor = system.actorOf(Props(new PullActor(asyncRequestHandler, httpResponseOutput)))
				asyncRequestHandler.latch.await(200, TimeUnit.MILLISECONDS)
				assert(httpResponseOutput.status === NotImplemented)
				assert(httpResponseOutput.contentType === "")
				val text = httpResponseOutput.text
				assert(text.contains(""))
			}
			
			"handle timeout" in {
				val httpResponseOutput = new HttpResponseOutputMock()
				val asyncRequestHandler = new AsyncRequestHandlerMock(httpResponseOutput)
				val pullActor = system.actorOf(Props(new PullActor(asyncRequestHandler, httpResponseOutput)))
				awaitCond(!asyncRequestHandler.timeoutListeners.isEmpty, 200 millis, 10 millis)
				asyncRequestHandler.sendTimeout()
				awaitCond(pullActor.isTerminated, 200 millis, 10 millis)
			}
			"handle error" in {
				val httpResponseOutput = new HttpResponseOutputMock()
				val asyncRequestHandler = new AsyncRequestHandlerMock(httpResponseOutput)
				val pullActor = system.actorOf(Props(new PullActor(asyncRequestHandler, httpResponseOutput)))
				awaitCond(!asyncRequestHandler.errorListeners.isEmpty, 200 millis, 10 millis)
				asyncRequestHandler.sendError()
				awaitCond(pullActor.isTerminated, 200 millis, 10 millis)
			}
		}
	}
	
	override def afterAll {
    system.shutdown()
  }
}