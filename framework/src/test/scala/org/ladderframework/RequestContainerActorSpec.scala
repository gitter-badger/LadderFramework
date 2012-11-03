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
import bootstrap.LadderBoot

class ResponseContainerActorSpec (system: ActorSystem) extends TestKit(system) with WordSpec with GivenWhenThen with BeforeAndAfterAll{
	
	implicit val webSystem = system
	
	def this() = this(ActorSystem("WebSystem"))

	"The request container actor" when {
		"handle time to live" should {
			"handle timing out" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				val uuid = Utils.uuid
				LadderBoot.timeToLivePage = 200
				val initalResponseContainer = system.actorOf(Props(new InitalResponseContainer(null, null, uuid)))
				awaitCond(initalResponseContainer.isTerminated, 500 millis, 25 millis)
			}
		}
	}
	
	override def afterAll {
    system.shutdown()
  }
}