package org.ladderframework

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

class ServiceSpec(system: ActorSystem) extends TestKit(system) with WordSpec with GivenWhenThen with BeforeAndAfterAll{

	def this() = this(ActorSystem("WebSystem"))
	
	val helloWorldResponse = HtmlResponse("<html><body>hello world</body></html>")
	
	val sessionID = "sessionID"
	val master = system.actorOf(Props[Master], name = "master")
	
	override def beforeAll {
		LadderBoot.site = {
			case HttpRequest(GET, _, "hello" :: "world" :: Nil, params, _) if params.size == 0 => helloWorldResponse
			case HttpRequest(POST | GET, _, "hello" :: "world" :: Nil, params, _) => 
				HtmlResponse("<html><body>hello world " + params("parameter").head + "</body></html>")
			case HttpRequest(GET, _, "resource" :: static :: Nil, _, _) =>
				println("HttpResourceResponse: " + static)
				HttpResourceResponse(path = static :: Nil)
			case HttpRequest(GET, _, "statefull" :: "request" :: Nil, _, _) =>
				new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context) = {
						val callback = context.addSubmitCallback(() => (
								"some" :: "where" :: "new" :: Nil, 
								HtmlResponse("<html><body>redirect post</body></html>"))
						)
						callback.tail.mkString("_")
					}
				}
			case HttpRequest(GET, _, originalPath @ "statefull" :: "ajaxrequest" :: Nil, _, _) =>
				new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context) = {
						val callback = context.addAjaxSubmitCallback(() => JsCall("callback"))
						callback.tail.mkString("_")
					}
				}
				
			case HttpRequest(GET, _, originalPath @ "statefull" :: "ajaxcallback" :: Nil, _, _) =>
				new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context) = {
						val callback = context.addAjaxInputCallback((input) => JsCall("inputCallback:" + input))
						callback.tail.mkString("_")
					}
				}
			case HttpRequest(GET, _, originalPath @ "statefull" :: "ajaxhandler" :: Nil, _, _) =>
				new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context) = {
						val callback = context.addAjaxHandlerCallback({
							case _ => HtmlResponse("got back here")
						})
						callback.tail.mkString("_")
					}
				}
			case HttpRequest(GET, _, originalPath @ "statefull" :: "pull" :: Nil, _, _) =>
				new StatefulHtmlResponse{
					override def statefullContent(implicit context:Context) = {
						context.update("Message")
						context.contextID
					}
				}
		}
		
		LadderBoot.resourceAsStream = getClass.getClassLoader().getResourceAsStream
		LadderBoot.resource = getClass.getClassLoader().getResource
		LadderBoot.mimeType = _ match {
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
	
	"The framework" when {
		"recieving a HTTP GET request to an in memory response (Hello World)" should {
			val request = HttpRequest(GET, sessionID,  "hello" :: "world" :: Nil)
			
			"handle simple request" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				send(HttpInteraction(asyncContext, request, httpServletResponse))
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.text === "<html><body>hello world</body></html>")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
			"handle bad request" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				val request = HttpRequest(GET, sessionID,  "hello" :: "not" :: "found" :: Nil)
				send(HttpInteraction(asyncContext, request, httpServletResponse))
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.text === NotFoundResponse.content)
				assert(httpServletResponse.getStatus === NotFound.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"recieving a HTTP GET request with parameter to an in memory response" should {
			val request = HttpRequest(GET, sessionID,  "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))
			
			"handle simple request" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				send(HttpInteraction(asyncContext, request, httpServletResponse))
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.text === "<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"recieving a HTTP GET request to a static resource on the file system" should {
			"serve valid requests" in {
				val asyncContext = new AsyncContextMock
				val httpServletResponse = new HttpServletResponseMock()
				val request = HttpRequest(GET, sessionID, "resource" :: "static.html" :: Nil)
				send(HttpInteraction(asyncContext, request, httpServletResponse))
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.text === "static")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
			
			"handle not found (404)" in {
				val asyncContext = new AsyncContextMock
				val httpServletResponse = new HttpServletResponseMock()
				val request = HttpRequest(GET, sessionID, "resource" :: "notFound.html" :: Nil)
				master ! HttpInteraction(asyncContext, request, httpServletResponse)
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.text === NotFoundResponse.content)
				assert(httpServletResponse.getStatus === NotFound.code)
				assert(httpServletResponse.contentType === "text/html")
			}
			
			"handle it asyncrounously" in {
				val asyncContext = new AsyncContextMock(10)
				val httpServletResponse = new HttpServletResponseMock()
				val request = HttpRequest(GET, sessionID, "resource" :: "static.html" :: Nil)
				for(i <- 0 to 10){
					master ! HttpInteraction(asyncContext, request, new HttpServletResponseMock())
				}
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(asyncContext.latch.getCount === 0)
			}
		}
		"recieving a HTTP POST request to an in memory response (Hello World time)" should {
			val request = HttpRequest(POST, sessionID,  "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))
			
			"handle simple request" in {
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				master ! HttpInteraction(asyncContext, request, httpServletResponse)
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.text === "<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}
		
		"handle statefull response" should {
			"handle POST REDIRECT GET" in {
				val request = HttpRequest(GET, sessionID,  "statefull" :: "request" :: Nil)
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				master ! HttpInteraction(asyncContext, request, httpServletResponse)
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val responseText = httpServletResponse.text.split("_")
				val stateful = responseText(0)
				val func = responseText(1)
				
				val httpServletResponseStatefull = new HttpServletResponseMock()
				val asyncContextStatefull = new AsyncContextMock
				val statefullRequest = HttpRequest(POST, sessionID,  "post" :: stateful :: func :: Nil, Map("key" -> Array("value")))
				master ! HttpInteraction(asyncContextStatefull, statefullRequest, httpServletResponseStatefull)
				asyncContextStatefull.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponseStatefull.getStatus === Found.code)
				val location = httpServletResponseStatefull.headers("Location")
				assert(location.startsWith(List("some", "where", "new").mkString("/", "/", "")))
				
				val param = location.split("\\?")(1).split("=")
				val httpServletResponseRedirect = new HttpServletResponseMock()
				val asyncContextRedirect = new AsyncContextMock
				val redirectRequest = HttpRequest(GET, sessionID,  List("some", "where", "new"), Map(param(0) -> param.tail))
				master ! HttpInteraction(asyncContextRedirect, redirectRequest, httpServletResponseRedirect)
				asyncContextRedirect.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponseRedirect.getStatus === OK.code)
				assert(httpServletResponseRedirect.contentType === "text/html")
				assert(httpServletResponseRedirect.text === "<html><body>redirect post</body></html>")
			}
			"handle simple ajax post" in {
				val request = HttpRequest(GET, sessionID,  "statefull" :: "ajaxrequest" :: Nil)
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				master ! HttpInteraction(asyncContext, request, httpServletResponse)
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val id = text(1)
				
				val httpServletResponseAjax = new HttpServletResponseMock()
				val asyncContextStatefull = new AsyncContextMock
				val statefullRequest = HttpRequest(POST, sessionID,  "ajax" :: statefull :: id :: Nil, Map("key" -> Array("value")))
				master ! HttpInteraction(asyncContextStatefull, statefullRequest, httpServletResponseAjax)
				asyncContextStatefull.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponseAjax.text === "callback();")
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/javascript")
			}
			
			"handle simple ajax callback" in {
				val request = HttpRequest(GET, sessionID,  "statefull" :: "ajaxcallback" :: Nil)
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				master ! HttpInteraction(asyncContext, request, httpServletResponse)
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val key = text(1)
				
				val httpServletResponseAjax = new HttpServletResponseMock()
				val asyncContextStatefull = new AsyncContextMock
				val statefullRequest = HttpRequest(POST, sessionID,  "ajax" :: statefull :: Nil, Map(key -> Array("inputValue")))
				master ! HttpInteraction(asyncContextStatefull, statefullRequest, httpServletResponseAjax)
				asyncContextStatefull.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/javascript")
				assert(httpServletResponseAjax.text === "inputCallback:inputValue();")
			}
			"handle simple ajax callback handler" in {
				val request = HttpRequest(GET, sessionID,  "statefull" :: "ajaxhandler" :: Nil)
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				master ! HttpInteraction(asyncContext, request, httpServletResponse)
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.split("_")
				val statefull = text(0)
				val key = text(1)
				
				val httpServletResponseAjax = new HttpServletResponseMock()
				val asyncContextStatefull = new AsyncContextMock
				val statefullRequest = HttpRequest(POST, sessionID,  "ajax" :: statefull :: key :: Nil, Map(key -> Array("inputValue")))
				master ! HttpInteraction(asyncContextStatefull, statefullRequest, httpServletResponseAjax)
				asyncContextStatefull.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponseAjax.getStatus === OK.code)
				assert(httpServletResponseAjax.contentType === "text/html")
				assert(httpServletResponseAjax.text === "got back here")
			}
		}
		
		"handle polling" should {
			"handle pushing messages" in {
				val request = HttpRequest(GET, sessionID,  "statefull" :: "pull" :: Nil)
				val httpServletResponse = new HttpServletResponseMock()
				val asyncContext = new AsyncContextMock
				master ! HttpInteraction(asyncContext, request, httpServletResponse)
				asyncContext.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponse.getStatus === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val statefull = httpServletResponse.text
				
				val httpServletResponsePull = new HttpServletResponseMock()
				val asyncContextPull = new AsyncContextMock
				val pullRequest = HttpRequest(POST, sessionID,  "pull" :: statefull :: Nil)
				master ! HttpInteraction(asyncContextPull, pullRequest, httpServletResponsePull)
				asyncContextPull.latch.await(1, TimeUnit.SECONDS)
				assert(httpServletResponsePull.getStatus === OK.code)
				assert(httpServletResponsePull.contentType === "text/json")
				val RegExp = """\[\{id:"(.*)", message:"Message"\}\]""".r
				httpServletResponsePull.text match {
					case RegExp(id) => 
					case _ => assert(false, httpServletResponsePull.text + " --- " + RegExp) 
				}
			}
		}
	} 
}