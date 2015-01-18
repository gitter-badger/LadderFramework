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
import org.scalatest.time.Span
import org.scalatest.time.Millis
import org.scalatest.Tag
import org.ladderframework.html.form.Form
import org.ladderframework.html.form.Forms._
import org.ladderframework.html.form.Formats._
import scala.concurrent.ExecutionContext
import scala.xml.NodeSeq
import org.ladderframework.js.JsNoCmd
import org.ladderframework.html.form.Ajax
import org.ladderframework.html.form.StatefulForm
import org.ladderframework.html.form.StatefulPost

object LocalTest extends Tag("org.ladderframework.LocalTest")

class LadderServerSpec (system: ActorSystem) extends TestKit(system) with FunSpecLike with BeforeAndAfterAll with ScalaFutures{
	
	implicit val webSystem = system
	
	def this() = this(ActorSystem("LadderServerSpecSystem"))

	implicit val patience = PatienceConfig(timeout = scaled(Span(1000, Millis)))
	
	describe("LadderServer")  {
		pending
	 	it ("should from server", LocalTest) {
			val server = new LadderServer(new DefaultBoot{
				def site = {
					case _ => Future.successful(XmlResponse(<div>Jeg er glad!!</div>))
				}
			})
			
			server.start("127.0.0.1", 23023)
			
			Thread.sleep(2500)
			val url = new URL("http://localhost:23023/Jalla")
			
			val content = Future{
				io.Source.fromURL(url, "UTF-8").getLines.mkString
			}.futureValue
			println("content: " + content)
			assert(content.contains("Jeg er glad!!"))
			server.stop()
		}
		it ("should handle static file", LocalTest){
			val server = new LadderServer(new DefaultBoot{
				def site = {
					case Method.GET(Path("static.html" :: Nil)) => Future.successful(HttpResourceResponse(path = List("static.html")))
				}
			})
			
			server.start("127.0.0.1", 23024)
			
			Thread.sleep(2500)
			val url = new URL("http://localhost:23024/static.html")
			
			val content = Future{
				io.Source.fromURL(url, "UTF-8").getLines.mkString
			}.futureValue
			println("content: " + content)
			assert(content.contains("static"))
			server.stop()
		}
		
		it ("should handle form"){
			val server = new LadderServer(new DefaultBoot{
				def site = {
					case Method.GET(Path("form" :: Nil)) => Future.successful(new SomeDataHtmlPage(None))
				}
			})
			
			server.start("127.0.0.1", 23025)
			
			Thread.sleep(2500)
			val url = new URL("http://localhost:23025/form")
			Thread.sleep(25000)
			val content = Future{
				io.Source.fromURL(url, "UTF-8").getLines.mkString
			}.futureValue
			println("content: " + content)
			assert(content.contains("static"))
			server.stop()
		}
	}

	override def afterAll:Unit = {
    system.shutdown()
  }
	
}

case class SomeData(name: String)

class SomeDataHtmlPage(someData: Option[SomeData]) extends StatefulHtmlPage {
	val source: String = "somedata.html"
		
	val form = Form(mapping(SomeData)(SomeData.unapply _)(
		of[String]
	))
	
	def render(implicit context: Context, ec: ExecutionContext): Future[NodeSeq => NodeSeq] = Future{
		StatefulPost(form)((either, id) => Future(("static.html" :: Nil, new SomeDataHtmlPage(either.right.toOption))))(
				implicit context => someDataForm => {
					ns => {
						<div>
							<h3>Speaker</h3>
							{
								someDataForm.render(name => _ => name.text("Name", 'id -> "name"))
							}
							<input type="submit" value="Send"/>
						</div>
					}
				}
		)
		ns => ns
	}
}