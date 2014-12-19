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
import org.scalatest.WordSpecLike
import akka.testkit.TestProbe
import scala.concurrent.Promise

class ResponseContainerActorSpec (system: ActorSystem) extends TestKit(system) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll{
	
	implicit val webSystem = system
	
	def this() = this(ActorSystem("WebSystem"))
	
	val boot = new DefaultBoot{
		def site = {
			case in => ??? 
		}
		override val timeToLivePage = 200
	} 

	"The request container actor" when {
		"handle time to live" should {
			"handle timing out" in {
				val httpResponseOutput = Promise[HttpResponseOutput]()
				val uuid = Utils.uuid
				val initalResponseContainer = system.actorOf(Props(new InitalResponseContainer(null, null, uuid, boot)))
				val probe = TestProbe()
				probe watch initalResponseContainer
//				asyncRequestHandler.sendError()
				probe.expectTerminated(initalResponseContainer)
			}
		}
	}
	
	override def afterAll: Unit = {
    system.shutdown()
  }
}