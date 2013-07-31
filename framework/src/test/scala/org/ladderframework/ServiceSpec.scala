package org.ladderframework

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success
import com.typesafe.config.ConfigFactory
import org.scalatest.WordSpec
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfterAll
import org.ladderframework.mock._
import bootstrap.LadderBoot
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.PoisonPill
import akka.routing.RoundRobinRouter
import akka.testkit.TestKit
import javax.servlet.http.HttpServletResponse
import java.util.concurrent.TimeUnit
import org.ladderframework.js.JsCall
import org.ladderframework.js.JsCmd
import org.junit.runner.RunWith
import org.scalatest.WordSpecLike

class ServiceSpec(system: ActorSystem) extends TestKit(system) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll{

	def this() = this(ActorSystem("WebSystem"))
	
	val helloWorldResponse = Future(HtmlResponse("<html><body>hello world</body></html>"))
	val requestHandler = system.actorOf(Props[RequestHandler].withRouter(RoundRobinRouter(10)), name = "requestHandler")
	
	val sessionID = "sessionID"
	
	override def beforeAll {
		LadderBoot.site = {
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
		
		LadderBoot.resourceAsStreamImpl = (in:String) => getClass.getClassLoader().getResourceAsStream(in.substring(1))
		LadderBoot.resourceImpl = (in:String) => getClass.getClassLoader().getResource(in.substring(1))
		LadderBoot.mimeTypeImpl = _ match {
			case s:String if s.endsWith("html") => "text/html"
			case _ => "NOT FOUND"
		}
		
		system.actorOf(SessionActor(sessionID), name = sessionID)
  }
	
	def send(msg:Any) {
		requestHandler ! msg 
	}
		
	override def afterAll {
		system.actorFor(sessionID:: Nil) ! PoisonPill
    system.shutdown()
  }
	
	def call(request: HttpRequest):HttpServletResponseMock = {
		val httpServletResponse = new HttpServletResponseMock()
		val asyncContext = new AsyncContextMock(httpServletResponse)
		send(HttpInteraction(asyncContext, request, httpServletResponse))
		assert(asyncContext.latch.await(2, TimeUnit.SECONDS))
		httpServletResponse
	}
	
	def httpRequest(givenMethod: Method, givenSession: String, givenPath: List[String], givenParams: Map[String, Array[String]] = Map()) = new HttpRequest{
		override val method = givenMethod
		override val sessionID = givenSession
		override def path = givenPath
		override def parameters = givenParams
	}
	
	"The framework" when {
		"recieving a HTTP GET request to an in memory response (Hello World)" should {
			
			"handle simple request" in {
				val httpServletResponse = call(httpRequest(GET, sessionID,  "hello" :: "world" :: Nil))
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.text === "<html><body>hello world</body></html>")
				assert(httpServletResponse.contentType === "text/html")
			}
			"handle bad request" in {
				val httpServletResponse = call(httpRequest(GET, sessionID,  "hello" :: "not" :: "found" :: Nil))
				val content = Await.result(NotFoundResponse.content, 2 seconds)
				assert(httpServletResponse.getStatus === NotFound.code)
				assert(httpServletResponse.text === content)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"recieving a HTTP GET request with parameter to an in memory response" should {
			val request = httpRequest(GET, sessionID, "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))
			
			"handle simple request" in {
				val httpServletResponse = call(request)
				assert(httpServletResponse.text === "<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"recieving a HTTP GET request to a static resource on the file system" should {
			"serve valid requests" in {
				val httpServletResponse = call(httpRequest(GET, sessionID, "resources" :: "static.html" :: Nil))
				assert(httpServletResponse.text === "static")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
			
			"handle not found (404)" in {
				val httpServletResponse = call(httpRequest(GET, sessionID, "resources" :: "notFound.html" :: Nil))
				val content = Await.result(NotFoundResponse.content, 2 seconds)
				assert(httpServletResponse.text === content)
				assert(httpServletResponse.getStatus === NotFound.code)
				assert(httpServletResponse.contentType === "text/html")
			}
			
			"handle it asyncrounously" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock(httpServletResponse, 10)
				val request = httpRequest(GET, sessionID, "resources" :: "static.html" :: Nil)
				for(i <- 0 to 10){
					requestHandler ! HttpInteraction(asyncContext, request, httpServletResponse)
				}
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(asyncContext.latch.getCount === 0)
			}
		}
		"recieving a HTTP POST request to an in memory response (Hello World time)" should {
			val request = httpRequest(POST, sessionID,  "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))
			
			"handle simple request" in {
				val httpServletResponse = call(request)
				assert(httpServletResponse.text === "<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"handle statefull response" should {
			"handle POST REDIRECT GET" in {
				val httpServletResponse = call(httpRequest(GET, sessionID,  "statefull" :: "request" :: Nil))
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val responseText = httpServletResponse.text.split("_")
				val stateful = responseText(0)
				val func = responseText(1)
				
				val httpServletResponseStatefull = call(httpRequest(POST, sessionID,  "post" :: stateful :: func :: Nil, Map("key" -> Array("value"))))
				assert(httpServletResponseStatefull.getStatus === Found.code)
				val location = httpServletResponseStatefull.headers("Location")
				assert(location.startsWith(List("some", "where", "new").mkString("/", "/", "")))
				
				val param = location.split("\\?")(1).split("=")
				val httpServletResponseRedirect = call( httpRequest(GET, sessionID, List("some", "where", "new"), Map(param(0) -> param.tail)))
				assert(httpServletResponseRedirect.getStatus === OK.code)
				assert(httpServletResponseRedirect.contentType === "text/html")
				assert(httpServletResponseRedirect.text === "<html><body>redirect post</body></html>")
			}
			"handle simple ajax post" in {
				val httpServletResponse = call(httpRequest(GET, sessionID,  "statefull" :: "ajaxrequest" :: Nil))
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val id = text(1)
				
				val httpServletResponseAjax = call( httpRequest(POST, sessionID, "ajax" :: statefull :: id :: Nil, Map("key" -> Array("value"))))
				assert(httpServletResponseAjax.text === "callback();")
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/javascript")
			}
			
			"handle simple ajax callback" in {
				val request = httpRequest(GET, sessionID, "statefull" :: "ajaxcallback" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val key = text(1)
				
				val httpServletResponseAjax = call(httpRequest(POST, sessionID,  "ajax" :: statefull :: Nil, Map(key -> Array("inputValue"))))
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/javascript")
				assert(httpServletResponseAjax.text === "inputCallback:inputValue();")
			}
			"handle simple ajax callback handler" in {
				val request = httpRequest(GET, sessionID, "statefull" :: "ajaxhandler" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val key = text(1)
				
				val httpServletResponseAjax = call(httpRequest(POST, sessionID,  "ajax" :: statefull :: key :: Nil, Map(key -> Array("inputValue"))))
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/html")
				assert(httpServletResponseAjax.text === "got back here")
			}
		}
		
		"handle polling" should {
			"handle pushing messages" in {
				val request = httpRequest(GET, sessionID,  "statefull" :: "pull" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val statefull = httpServletResponse.text
				
				val httpServletResponsePull = call(httpRequest(POST, sessionID,  "pull" :: statefull :: Nil))
				assert(httpServletResponsePull.getStatus === OK.code)
				assert(httpServletResponsePull.contentType === "text/json")
				val RegExp = """\{"messages":\[\{"id":"(.*)", "message":"Message\(\);"\}\]\}""".r
				httpServletResponsePull.text match {
					case RegExp(id) => 
					case _ => assert(false, httpServletResponsePull.text + " --- " + RegExp) 
				}
			}
		}
	} 
}