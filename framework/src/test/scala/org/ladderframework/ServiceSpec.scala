package org.ladderframework

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import org.scalatest.WordSpec
import org.scalatest.GivenWhenThen
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Props
import org.scalatest.BeforeAndAfterAll
import akka.testkit.TestKit
import org.ladderframework.mock._
import java.util.concurrent.TimeUnit
import bootstrap.LadderBoot
import akka.actor.ActorRef
import akka.actor.Actor
import org.ladderframework.js.JsCall
import org.ladderframework.js.JsCmd
import org.junit.runner.RunWith
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success
import javax.servlet.http.HttpServletResponse

class ServiceSpec(system: ActorSystem) extends TestKit(system) with WordSpec with GivenWhenThen with BeforeAndAfterAll{

	def this() = this(ActorSystem("WebSystem"))
	
	val helloWorldResponse = Future(HtmlResponse("<html><body>hello world</body></html>"))
	
	val sessionID = "sessionID"
	val master = system.actorOf(Props[Master], name = "master")
	
	override def beforeAll {
		LadderBoot.site = {
			case HttpRequest(GET, _, "hello" :: "world" :: Nil, params, _) if params.size == 0 => helloWorldResponse
			case HttpRequest(POST | GET, _, "hello" :: "world" :: Nil, params, _) => 
				Future(HtmlResponse("<html><body>hello world " + params("parameter").head + "</body></html>"))
			case HttpRequest(GET, _, "resources" :: static :: Nil, _, _) =>
				println("HttpResourceResponse: " + static)
				Future(HttpResourceResponse(path = static :: Nil))
			case HttpRequest(GET, _, "statefull" :: "request" :: Nil, _, _) =>
				Future(new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
						val callback = context.addSubmitCallback(params => Future(
								"some" :: "where" :: "new" :: Nil, 
								HtmlResponse("<html><body>redirect post</body></html>")
						))
						callback.tail.mkString("_")
					}
				})
			case HttpRequest(GET, _, originalPath @ "statefull" :: "ajaxrequest" :: Nil, _, _) =>
				Future(new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
						val callback = context.addAjaxFormSubmitCallback(params => Future(JsCall("callback")))
						callback.split("/").tail.mkString("_")
					}
				})
			case HttpRequest(GET, _, originalPath @ "statefull" :: "ajaxcallback" :: Nil, _, _) =>
				Future(new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
						val callback = context.addAjaxInputCallback((input) => Future(JsCall("inputCallback:" + input)))
						callback.lookupPath.tail.mkString("_")
					}
				})
			case HttpRequest(GET, _, originalPath @ "statefull" :: "ajaxhandler" :: Nil, _, _) =>
				Future(new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context, ec:ExecutionContext) = Future{
						val callback = context.addAjaxHandlerCallback({
							case _ => Future(HtmlResponse("got back here"))
						})
						callback.lookupPath.tail.mkString("_")
					}
				})
			case HttpRequest(GET, _, originalPath @ "statefull" :: "pull" :: Nil, _, _) =>
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
		
		master ! CreateSession(sessionID)
  }
	
	def send(msg:Any) {
		system.actorFor(master.path / "requestHandler") ! msg 
	}
		
	override def afterAll {
		master ! RemoveSession(sessionID)
    system.shutdown()
  }
	
	def call(request: HttpRequest):HttpServletResponseMock = {
		val httpServletResponse = new HttpServletResponseMock()
		val asyncContext = new AsyncContextMock(httpServletResponse)
		send(HttpInteraction(asyncContext, request, httpServletResponse))
		asyncContext.latch.await(1, TimeUnit.SECONDS)
		httpServletResponse
	}
	
	"The framework" when {
		"recieving a HTTP GET request to an in memory response (Hello World)" should {
			
			"handle simple request" in {
				val httpServletResponse = call(HttpRequest(GET, sessionID,  "hello" :: "world" :: Nil))
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.text === "<html><body>hello world</body></html>")
				assert(httpServletResponse.contentType === "text/html")
			}
			"handle bad request" in {
				val httpServletResponse = call(HttpRequest(GET, sessionID,  "hello" :: "not" :: "found" :: Nil))
				val content = Await.result(NotFoundResponse.content, 2 seconds)
				assert(httpServletResponse.getStatus === NotFound.code)
				assert(httpServletResponse.text === content)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"recieving a HTTP GET request with parameter to an in memory response" should {
			val request = HttpRequest(GET, sessionID,  "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))
			
			"handle simple request" in {
				val httpServletResponse = call(request)
				assert(httpServletResponse.text === "<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"recieving a HTTP GET request to a static resource on the file system" should {
			"serve valid requests" in {
				val httpServletResponse = call(HttpRequest(GET, sessionID, "resources" :: "static.html" :: Nil))
				assert(httpServletResponse.text === "static")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
			
			"handle not found (404)" in {
				val httpServletResponse = call(HttpRequest(GET, sessionID, "resources" :: "notFound.html" :: Nil))
				val content = Await.result(NotFoundResponse.content, 2 seconds)
				assert(httpServletResponse.text === content)
				assert(httpServletResponse.getStatus === NotFound.code)
				assert(httpServletResponse.contentType === "text/html")
			}
			
			"handle it asyncrounously" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock(httpServletResponse, 10)
				val request = HttpRequest(GET, sessionID, "resources" :: "static.html" :: Nil)
				for(i <- 0 to 10){
					master ! HttpInteraction(asyncContext, request, httpServletResponse)
				}
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(asyncContext.latch.getCount === 0)
			}
		}
		"recieving a HTTP POST request to an in memory response (Hello World time)" should {
			val request = HttpRequest(POST, sessionID,  "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))
			
			"handle simple request" in {
				val httpServletResponse = call(request)
				assert(httpServletResponse.text === "<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"handle statefull response" should {
			"handle POST REDIRECT GET" in {
				val httpServletResponse = call(HttpRequest(GET, sessionID,  "statefull" :: "request" :: Nil))
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val responseText = httpServletResponse.text.split("_")
				val stateful = responseText(0)
				val func = responseText(1)
				
				val httpServletResponseStatefull = call(HttpRequest(POST, sessionID,  "post" :: stateful :: func :: Nil, Map("key" -> Array("value"))))
				assert(httpServletResponseStatefull.getStatus === Found.code)
				val location = httpServletResponseStatefull.headers("Location")
				assert(location.startsWith(List("some", "where", "new").mkString("/", "/", "")))
				
				val param = location.split("\\?")(1).split("=")
				val httpServletResponseRedirect = call( HttpRequest(GET, sessionID,  List("some", "where", "new"), Map(param(0) -> param.tail)))
				assert(httpServletResponseRedirect.getStatus === OK.code)
				assert(httpServletResponseRedirect.contentType === "text/html")
				assert(httpServletResponseRedirect.text === "<html><body>redirect post</body></html>")
			}
			"handle simple ajax post" in {
				val httpServletResponse = call(HttpRequest(GET, sessionID,  "statefull" :: "ajaxrequest" :: Nil))
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val id = text(1)
				
				val httpServletResponseAjax = call( HttpRequest(POST, sessionID,  "ajax" :: statefull :: id :: Nil, Map("key" -> Array("value"))))
				assert(httpServletResponseAjax.text === "callback();")
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/javascript")
			}
			
			"handle simple ajax callback" in {
				val request = HttpRequest(GET, sessionID,  "statefull" :: "ajaxcallback" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val key = text(1)
				
				val httpServletResponseAjax = call(HttpRequest(POST, sessionID,  "ajax" :: statefull :: Nil, Map(key -> Array("inputValue"))))
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/javascript")
				assert(httpServletResponseAjax.text === "inputCallback:inputValue();")
			}
			"handle simple ajax callback handler" in {
				val request = HttpRequest(GET, sessionID,  "statefull" :: "ajaxhandler" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val key = text(1)
				
				val httpServletResponseAjax = call(HttpRequest(POST, sessionID,  "ajax" :: statefull :: key :: Nil, Map(key -> Array("inputValue"))))
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/html")
				assert(httpServletResponseAjax.text === "got back here")
			}
		}
		
		"handle polling" should {
			"handle pushing messages" in {
				val request = HttpRequest(GET, sessionID,  "statefull" :: "pull" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val statefull = httpServletResponse.text
				
				val httpServletResponsePull = call(HttpRequest(POST, sessionID,  "pull" :: statefull :: Nil))
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