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
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.Span
import org.scalatest.time.Millis
import Status.OK

class PullActorSpec (system: ActorSystem) extends TestKit(system) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll with ScalaFutures{
	
	implicit val patience = PatienceConfig(timeout = scaled(Span(1000, Millis)))
	
	implicit val webSystem = system
	
	val boot = new DefaultBoot{
		override def site = {
			case _ => ???
		}
	}
	
	def this() = this(ActorSystem("PullActorSpecSystem"))

	"The pull actor" when {
		"handle polling" should {
			"handle pushing message" in {
				val httpResponseOutput = Promise[HttpResponseOutput]()
				val pullActor = system.actorOf(PullActor(httpResponseOutput, boot), "pushMessage")
				val uuid = Utils.uuid
				val msg = "sendSimpleMessage"
				pullActor ! PushMessage(uuid, msg)
				whenReady(httpResponseOutput.future)(httpResponseOutput => { 
					assert(httpResponseOutput.status === OK)
					assert(httpResponseOutput.contentType === ContentType.`application/json`)
					val text = httpResponseOutput.content.toString
					assert(text.contains(uuid))
					assert(text.contains(msg))
				})
			}
			
			"handle pushing messages" in {
				val httpResponseOutput = Promise[HttpResponseOutput]()
				val pullActor = system.actorOf(PullActor(httpResponseOutput, boot))
				val uuid = Utils.uuid
				val uuid2 = Utils.uuid
				val message1 = "sendMessage1"
				val message2 = "sendMessage2"
				pullActor ! List(PushMessage(uuid, message1), PushMessage(uuid2, message2))
				whenReady(httpResponseOutput.future)(httpResponseOutput => {
					assert(httpResponseOutput.status === OK)
					assert(httpResponseOutput.contentType === ContentType.`application/json`)
					val text = httpResponseOutput.content.toString
					assert(text.contains(uuid))
					assert(text.contains(message1))
					assert(text.contains(uuid2))
					assert(text.contains(message2))
				})
			}
			
			"handle pushing no messages" in {
				val httpResponseOutput = Promise[HttpResponseOutput]()
				val pullActor = system.actorOf(PullActor(httpResponseOutput, boot))
				pullActor ! Nil
				assert(!httpResponseOutput.future.isReadyWithin(5 seconds))
			}
			
			"handle timeout" in {
				val httpResponseOutput = Promise[HttpResponseOutput]()
				val pullActor = TestActorRef(new PullActor(httpResponseOutput, boot))
				pullActor ! pullActor.underlyingActor.TimeToClose
				val probe = TestProbe()
				probe watch pullActor
				probe.expectTerminated(pullActor)
			}
		}
	}
	
	override def afterAll: Unit = {
    system.shutdown()
  }
}