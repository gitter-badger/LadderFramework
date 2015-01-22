package org.ladderframework

import java.io.InputStream
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import org.ladderframework.js.JsCall
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Finders
import org.scalatest.GivenWhenThen
import org.scalatest.WordSpecLike
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.Millis
import org.scalatest.time.Span
import Method.GET
import Method.POST
import akka.actor.ActorSelection.toScala
import akka.actor.ActorSystem
import akka.actor.PoisonPill
import akka.testkit.TestKit
import Status._
import javax.servlet.http.Part

class ServiceSpec(system: ActorSystem) extends TestKit(system) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll with ScalaFutures{

	def this() = this(ActorSystem("WebSystem"))
	
	implicit val patience = PatienceConfig(timeout = scaled(Span(500, Millis)))
	
	val helloWorldResponse = Future.successful(HtmlResponse("<html><body>hello world</body></html>"))
	
	val sessionID = SessionId("sessionID")
	
	override def beforeAll = {
		val boot = new DefaultBoot{
			override val site: PartialFunction[HttpRequest, Future[HttpResponse]] = {
				case req @ HttpRequest(GET, "hello" :: "world" :: Nil) if req.parameters.size == 0 => helloWorldResponse
				case req @ HttpRequest(POST | GET, "hello" :: "world" :: Nil) => 
					Future(HtmlResponse("<html><body>hello world " + req.parameters("parameter").head + "</body></html>"))
				case HttpRequest(GET, "resources" :: static :: Nil) =>
					//println("HttpResourceResponse: " + static)
					Future(HttpResourceResponse(path = static :: Nil))
				case HttpRequest(GET, "statefull" :: "request" :: Nil) =>
					Future(new StatefulHtmlResponse{
						override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
							val callback = context.addSubmitCallback(params => Future(
									"some" :: "where" :: "new" :: Nil, 
									HtmlResponse("<html><body>redirect post</body></html>")
							))
							callback.tail.mkString("_")
						}
					})
				case HttpRequest(GET, originalPath @ "statefull" :: "ajaxrequest" :: Nil) =>
					Future(new StatefulHtmlResponse{
						override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
							val callback = context.addAjaxFormSubmitCallback(params => Future(JsCall("callback")))
							callback.split("/").tail.mkString("_")
						}
					})
				case HttpRequest(GET, originalPath @ "statefull" :: "ajaxcallback" :: Nil) =>
					Future(new StatefulHtmlResponse{
						override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
							val callback = context.addAjaxInputCallback((input) => Future(JsCall("inputCallback:" + input)))
							callback.lookupPath.tail.mkString("_")
						}
					})
				case HttpRequest(GET, originalPath @ "statefull" :: "ajaxhandler" :: Nil) =>
					Future(new StatefulHtmlResponse{
						override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
							val callback = context.addAjaxHandlerCallback({
								case _ => Future(HtmlResponse("got back here"))
							})
							callback.lookupPath.tail.mkString("_")
						}
					})
				case HttpRequest(GET, originalPath @ "statefull" :: "pull" :: Nil) =>
					Future(new StatefulHtmlResponse{
						override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
							context.update(JsCall("Message"))
							context.contextID
						}
					})
			}
			
			override def resourceAsStream(in:String) = getClass.getClassLoader().getResourceAsStream(in.substring(1))
			override def resource(in:String) = getClass.getClassLoader().getResource(in.substring(1))
			override def mimeType(ending: String) = ending match {
				case s:String if s.endsWith("html") => "text/html"
				case _ => "NOT FOUND"
			}
		}
		
		system.actorOf(SessionActor(sessionID, boot), name = sessionID.value)
  }
	
	def send(msg:HttpInteraction) = {
		system.actorSelection(system / msg.req.sessionId.value) ! msg 
	}
		
	override def afterAll = {
		system.actorSelection(s"/user/${sessionID.value}") ! PoisonPill
    system.shutdown()
  }
	def call(request: HttpRequest):Future[HttpResponseOutput] = {
		val httpResponseOutput = Promise[HttpResponseOutput]()
		send(HttpInteraction(request, httpResponseOutput))
		httpResponseOutput.future
	}
	
	def httpRequest(givenMethod: Method, givenSession: SessionId, givenPath: List[String], givenParams: Map[String, Array[String]] = Map()) = new HttpRequest{
		override val method = givenMethod
		override val sessionId = givenSession
		override def path = givenPath
		override def headers = Map.empty
		override def parameters = givenParams
		override def cookies = Nil
		override def invalidateSession() = {}
	}
	
	"The framework" when {
		import ExecutionContext.Implicits.global
		"recieving a HTTP GET request to an in memory response (Hello World)" should {
			"handle simple request" in {
				val httpServletResponse = call(httpRequest(GET, sessionID,  "hello" :: "world" :: Nil))
				whenReady(httpServletResponse)(httpServletResponse => {
					assert(httpServletResponse.status === OK)
					assert(httpServletResponse.content.toString === "<html><body>hello world</body></html>")
					assert(httpServletResponse.contentType === ContentType.`text/html`)
				})
			}
			"handle bad request" in {
				val httpServletResponse = call(httpRequest(GET, sessionID,  "hello" :: "not" :: "found" :: Nil))
				whenReady(httpServletResponse)(httpServletResponse => {
					assert(httpServletResponse.status === NotFound)
					assert(httpServletResponse.content.toString === NotFoundDefaultResponse.content.futureValue)
					assert(httpServletResponse.contentType === ContentType.`text/html`)
				})
			}
		}
		
		"recieving a HTTP GET request with parameter to an in memory response" should {
			val request = httpRequest(GET, sessionID, "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))
			
			"handle simple request" in {
				val httpServletResponse = call(request)
				whenReady(httpServletResponse)(httpServletResponse => {
					assert(httpServletResponse.content.toString === "<html><body>hello world postedValue</body></html>")
					assert(httpServletResponse.status === OK)
					assert(httpServletResponse.contentType === ContentType.`text/html`)
				})
			}
		}
		
		"recieving a HTTP GET request to a static resource on the file system" should {
			"serve valid requests" in {
				val httpServletResponse = call(httpRequest(GET, sessionID, "resources" :: "static.html" :: Nil))
				whenReady(httpServletResponse)(httpServletResponse => {
					assert(io.Source.fromInputStream(httpServletResponse.content.asInstanceOf[InputStream], "UTF-8").mkString === "static")
					assert(httpServletResponse.status === OK)
					assert(httpServletResponse.contentType === ContentType.`text/html`)
				})
			}
			
			"handle not found (404)" in {
				val httpServletResponse = call(httpRequest(GET, sessionID, "resources" :: "notFound.html" :: Nil)).futureValue
				val content = NotFoundDefaultResponse.content.futureValue
				assert(httpServletResponse.content.toString === content)
				assert(httpServletResponse.status === NotFound)
				assert(httpServletResponse.contentType === ContentType.`text/html`)
			}
			
			"handle it asyncrounously" in {
				val httpResponseOutput = Promise[HttpResponseOutput]()
				val request = httpRequest(GET, sessionID, "resources" :: "static.html" :: Nil)
				for(i <- 0 to 10){
					send(HttpInteraction(request, httpResponseOutput))
				}
//				asyncRequestHandler.latch.await(1, TimeUnit.SECONDS)
//				assert(asyncRequestHandler.latch.getCount === 0)
			}
		}
		"recieving a HTTP POST request to an in memory response (Hello World time)" should {
			val request = httpRequest(POST, sessionID,  "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))
			
			"handle simple request" in {
				val httpServletResponse = call(request).futureValue
				assert(httpServletResponse.content.toString === "<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.status === OK)
				assert(httpServletResponse.contentType === ContentType.`text/html`)
			}
		}
		
		"handle statefull response" should {
			"handle POST REDIRECT GET" in {
				val httpServletResponse = call(httpRequest(GET, sessionID,  "statefull" :: "request" :: Nil)).futureValue
				assert(httpServletResponse.status === OK)
				assert(httpServletResponse.contentType === ContentType.`text/html`)
				val responseText = httpServletResponse.content.toString.split("_")
				val stateful = responseText(0)
				val func = responseText(1)
				
				val httpServletResponseStatefull = call(httpRequest(POST, sessionID,  "post" :: stateful :: func :: Nil, Map("key" -> Array("value")))).futureValue
				assert(httpServletResponseStatefull.status === Found)
				val location = httpServletResponseStatefull.headers.filter(_.header == Header.Location).head.value
				assert(location.startsWith(List("some", "where", "new").mkString("/", "/", "")))
				
				val param = location.split("\\?")(1).split("=")
				val httpServletResponseRedirect = call( httpRequest(GET, sessionID, List("some", "where", "new"), Map(param(0) -> Array(param(1))))).futureValue
				assert(httpServletResponseRedirect.status === OK)
				assert(httpServletResponseRedirect.contentType === ContentType.`text/html`)
				assert(httpServletResponseRedirect.content.toString === "<html><body>redirect post</body></html>")
			}
			"handle simple ajax post" in {
				val httpServletResponse = call(httpRequest(GET, sessionID,  "statefull" :: "ajaxrequest" :: Nil)).futureValue
				assert(httpServletResponse.status === OK)
				assert(httpServletResponse.contentType === ContentType.`text/html`)
				val text = httpServletResponse.content.toString.split("_")
				val statefull = text(0)
				val id = text(1)
				
				val httpServletResponseAjax = call( httpRequest(POST, sessionID, "ajax" :: statefull :: id :: Nil, Map("key" -> Array("value")))).futureValue
				assert(httpServletResponseAjax.content.toString === "callback();")
				assert(httpServletResponseAjax.status === OK)
				assert(httpServletResponseAjax.contentType === ContentType.`text/javascript`)
			}
			
			"handle simple ajax callback" in {
				val request = httpRequest(GET, sessionID, "statefull" :: "ajaxcallback" :: Nil)
				val httpServletResponse = call(request).futureValue
				assert(httpServletResponse.status === OK)
				assert(httpServletResponse.contentType === ContentType.`text/html`)
				val text = httpServletResponse.content.toString.split("_")
				val statefull = text(0)
				val key = text(1)
				
				val httpServletResponseAjax = call(httpRequest(POST, sessionID,  "ajax" :: statefull :: Nil, Map(key -> Array("inputValue")))).futureValue
				assert(httpServletResponseAjax.status === OK)
				assert(httpServletResponseAjax.contentType === ContentType.`text/javascript`)
				assert(httpServletResponseAjax.content.toString === "inputCallback:inputValue();")
			}
			"handle simple ajax callback handler" in {
				val request = httpRequest(GET, sessionID, "statefull" :: "ajaxhandler" :: Nil)
				val httpServletResponse = call(request).futureValue
				assert(httpServletResponse.status === OK)
				assert(httpServletResponse.contentType === ContentType.`text/html`)
				val text = httpServletResponse.content.toString.split("_")
				val statefull = text(0)
				val key = text(1)
				
				val httpServletResponseAjax = call(httpRequest(POST, sessionID,  "ajax" :: statefull :: key :: Nil, Map(key -> Array("inputValue")))).futureValue
				assert(httpServletResponseAjax.status === OK)
				assert(httpServletResponseAjax.contentType === ContentType.`text/html`)
				assert(httpServletResponseAjax.content.toString === "got back here")
			}
		}
		
		"handle polling" should {
			"handle pushing messages" in {
				val request = httpRequest(GET, sessionID,  "statefull" :: "pull" :: Nil)
				val httpServletResponse = call(request).futureValue
				assert(httpServletResponse.status === OK)
				assert(httpServletResponse.contentType === ContentType.`text/html`)
				val statefull = httpServletResponse.content.toString
				
				val httpServletResponsePull = call(httpRequest(POST, sessionID,  "pull" :: statefull :: Nil)).futureValue
				assert(httpServletResponsePull.status === OK)
				assert(httpServletResponsePull.contentType === ContentType.`application/json`)
				val RegExp = """\{"messages":\[\{"id":"(.*)", "message":"Message\(\);"\}\]\}""".r
				httpServletResponsePull.content.toString match {
					case RegExp(id) => 
					case _ => assert(false, httpServletResponsePull.content.toString + " --- " + RegExp) 
				}
			}
		}
	} 
}