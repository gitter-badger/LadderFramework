package org.ladderframework

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.WordSpecLike
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpecLike
import java.net.URL
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.concurrent.ScalaFutures
import java.net.HttpURLConnection

class LadderServerSpec (system: ActorSystem) extends TestKit(system) with FunSpecLike with BeforeAndAfterAll with ScalaFutures{
	
	implicit val webSystem = system
	
	def this() = this(ActorSystem("LadderServerSpecSystem"))

	describe("Ladder") {
		it("should read from server"){
			val server = new LadderServer(new DefaultBoot{
				def site = {
					case _ => Future.successful(XmlResponse(<div>Jeg er glad!!</div>))
				}
			})
			
			server.start("127.0.0.1", 23023)
			
			Thread.sleep(1500)
			val url = new URL("http://localhost:23023/Jalla")
			
			val content = Future{
				io.Source.fromURL(url, "UTF-8").getLines.mkString
			}.futureValue
			println("content: " + content)
			assert(content.contains("Jeg er glad!!"))
			server.stop()
		}
	}

	override def afterAll:Unit = {
    system.shutdown()
  }
	
}