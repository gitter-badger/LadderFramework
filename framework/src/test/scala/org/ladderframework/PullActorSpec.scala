package org.ladderframework

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestActorRef
import org.scalatest.WordSpec
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfterAll
import akka.actor.Props
import javax.servlet.AsyncContext
import org.ladderframework.mock.HttpServletResponseMock
import org.ladderframework.mock.AsyncContextMock
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import org.scalatest.WordSpecLike

class PullActorSpec (system: ActorSystem) extends TestKit(system) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll{
	
	implicit val webSystem = system
	
	def this() = this(ActorSystem("WebSystem"))

	"The pull actor" when {
		"handle polling" should {
			"handle pushing message" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock(httpServletResponse)
				val pullActor = system.actorOf(Props(new PullActor(asyncContext, httpServletResponse)))
				val uuid = Utils.uuid
				pullActor ! PushMessage(uuid, "sendMessage")
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/json")
				val text = httpServletResponse.text
				assert(text.contains(uuid))
				assert(text.contains("sendMessage"))
			}
			"handle pushing messages" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock(httpServletResponse)
				val pullActor = system.actorOf(Props(new PullActor(asyncContext, httpServletResponse)))
				val uuid = Utils.uuid
				val uuid2 = Utils.uuid
				val message1 = "sendMessage1"
				val message2 = "sendMessage2"
				pullActor ! List(PushMessage(uuid, message1), PushMessage(uuid2, message2))
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/json")
				val text = httpServletResponse.text
				assert(text.contains(uuid))
				assert(text.contains(message1))
				assert(text.contains(uuid2))
				assert(text.contains(message2))
			}
			
			"handle pushing no messages" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock(httpServletResponse)
				val pullActor = TestActorRef(new PullActor(asyncContext, httpServletResponse))
				asyncContext.latch.await(200, TimeUnit.MILLISECONDS)
				assert(httpServletResponse.getStatus === 0)
				assert(httpServletResponse.contentType === "")
				val text = httpServletResponse.text
				assert(text.contains(""))
			}
			
			"handle timeout" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock(httpServletResponse)
				val pullActor = TestActorRef(new PullActor(asyncContext, httpServletResponse))
				asyncContext.sendTimeout
				awaitCond(pullActor.isTerminated, 200 millis, 10 millis)
			}
			"handle error" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock(httpServletResponse)
				val pullActor = TestActorRef(new PullActor(asyncContext, httpServletResponse))
				asyncContext.sendError
				awaitCond(pullActor.isTerminated, 200 millis, 10 millis)
			}
		}
	}
	
	override def afterAll {
    system.shutdown()
  }
}