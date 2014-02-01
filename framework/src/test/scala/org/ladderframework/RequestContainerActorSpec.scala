package org.ladderframework

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestActorRef
import org.scalatest.WordSpec
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfterAll
import akka.actor.Props
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import bootstrap.LadderBoot
import org.scalatest.WordSpecLike
import org.ladderframework.mock.HttpResponseOutputMock
import org.ladderframework.mock.AsyncRequestHandlerMock
import akka.testkit.TestProbe

class ResponseContainerActorSpec (system: ActorSystem) extends TestKit(system) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll{
	
	implicit val webSystem = system
	
	def this() = this(ActorSystem("WebSystem"))

	"The request container actor" when {
		"handle time to live" should {
			"handle timing out" in {
				val httpResponseOutput = new HttpResponseOutputMock()
				val asyncRequestHandler = new AsyncRequestHandlerMock(httpResponseOutput)
				val uuid = Utils.uuid
				LadderBoot.timeToLivePage = 200
				val initalResponseContainer = system.actorOf(Props(new InitalResponseContainer(null, null, uuid)))
				val probe = TestProbe()
				probe watch initalResponseContainer
				asyncRequestHandler.sendError()
				probe.expectTerminated(initalResponseContainer)
			}
		}
	}
	
	override def afterAll {
    system.shutdown()
  }
}