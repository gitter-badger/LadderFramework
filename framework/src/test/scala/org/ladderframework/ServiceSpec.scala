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
import java.nio.file.Files
import org.ladderframework.mock.HttpServletResponseMock
import javax.servlet.http.HttpServletRequest
import java.util.concurrent.TimeUnit
import org.ladderframework.mock.HttpServletRequestMock
import java.util.concurrent.CountDownLatch
import javax.servlet.http.{ Cookie => jCookie }
import scala.xml.Text

class ServiceSpec(systm: ActorSystem) extends TestKit(systm) with WordSpecLike with GivenWhenThen with BeforeAndAfterAll with ScalaFutures {

	def this() = this(ActorSystem("WebSystem"))

	implicit val patience = PatienceConfig(timeout = scaled(Span(500, Millis)))

	val helloWorldResponse = Future.successful(HtmlResponse(<html><body>hello world</body></html>))

	val boot = new DefaultBoot {
		override val system = systm
		override val site: PartialFunction[HttpRequest, Future[HttpResponse]] = {
			case req @ HttpRequest(GET, "hello" :: "world" :: Nil) if req.parameters.size == 0 => helloWorldResponse
			case req @ HttpRequest(POST | GET, "hello" :: "world" :: Nil) =>
				Future(HtmlResponse(<html><body>hello world { req.parameters("parameter").head }</body></html>))
			case HttpRequest(GET, "resources" :: static :: Nil) =>
				//println("HttpResourceResponse: " + static)
				Future(HttpResourceResponse(path = static :: Nil))
			case HttpRequest(GET, "statefull" :: "request" :: Nil) =>
				Future(new StatefulHtmlResponse {
					override def statefullContent(implicit context: Context) = Future.successful {
						println("statefullContent")
						val callback = context.addSubmitCallback(params => Future.successful(
							"some" :: "where" :: "new" :: Nil,
							HtmlResponse(<html><body>redirect post</body></html>)))
						println("callback : " + callback)
						Text(callback.mkString("_"))
					}
				})
			case HttpRequest(GET, originalPath @ "statefull" :: "ajaxrequest" :: Nil) =>
				Future(new StatefulHtmlResponse {
					override def statefullContent(implicit context: Context) = Future.successful {
						val callback = context.addAjaxFormSubmitCallback(params => Future.successful(JsCall("callback")))
						Text(callback.split("/").tail.mkString("_"))
					}
				})
			case HttpRequest(GET, originalPath @ "statefull" :: "ajaxcallback" :: Nil) =>
				Future(new StatefulHtmlResponse {
					override def statefullContent(implicit context: Context) = Future.successful {
						val callback = context.addAjaxInputCallback((input) => Future.successful(JsCall("inputCallback:" + input)))
						Text(callback.lookupPath.tail.mkString("_"))
					}
				})
			case HttpRequest(GET, originalPath @ "statefull" :: "ajaxhandler" :: Nil) =>
				Future(new StatefulHtmlResponse {
					override def statefullContent(implicit context: Context) = Future.successful {
						val callback = context.addAjaxHandlerCallback({
							case _ => Future.successful(HtmlResponse(Text("got back here")))
						})
						Text(callback.lookupPath.tail.mkString("_"))
					}
				})
			case HttpRequest(GET, originalPath @ "statefull" :: "pull" :: Nil) =>
				Future(new StatefulHtmlResponse {
					override def statefullContent(implicit context: Context) = Future.successful {
						context.update(JsCall("Message"))
						Text(context.contextID)
					}
				})
		}

		override def resourceAsStream(in: String) = getClass.getClassLoader().getResourceAsStream(in.substring(1))
		override def resource(in: String) = getClass.getClassLoader().getResource(in.substring(1))
		override def mimeType(ending: String) = ending match {
			case s: String if s.endsWith("html") => "text/html"
			case _ => "NOT FOUND"
		}
	}

	override def afterAll = {
		system.shutdown()
	}
	//	def call(request: HttpRequest):Future[HttpResponseOutput] = {
	//		val httpResponseOutput = Promise[HttpResponseOutput]()
	//		send(HttpInteraction(request, httpResponseOutput))
	//		httpResponseOutput.future
	//	}
	def call(httpServletRequest: HttpServletRequest): HttpServletResponseMock = {
		val httpServletResponse = new HttpServletResponseMock()
		val latch = new CountDownLatch(1)
		system.actorOf(RequestHandler.create(boot, latch.countDown, httpServletRequest, httpServletResponse))
		assert(latch.await(2, TimeUnit.SECONDS))
		httpServletResponse
	}

	def httpRequest(givenMethod: Method, givenPath: List[String],
		givenParams: Map[String, Array[String]] = Map(), cookies: List[jCookie] = Nil): HttpServletRequest =
		new HttpServletRequestMock(givenMethod, givenPath, givenParams, cookies)

	"The framework" when {
		import ExecutionContext.Implicits.global
		"recieving a HTTP GET request to an in memory response (Hello World)" should {
			"handle simple request" in {
				val httpServletResponse = call(httpRequest(GET, "hello" :: "world" :: Nil))
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.text === "<!DOCTYPE html>\n<html><body>hello world</body></html>")
				assert(httpServletResponse.contentType === "text/html")
			}
			"handle bad request" in {
				val httpServletResponse = call(httpRequest(GET, "hello" :: "not" :: "found" :: Nil))
				val content = NotFoundDefaultResponse.content.futureValue
				assert(httpServletResponse.status === NotFound.code)
				assert(httpServletResponse.text === "<!DOCTYPE html>\n" + content)
				assert(httpServletResponse.contentType === "text/html")
			}
		}

		"recieving a HTTP GET request with parameter to an in memory response" should {
			val request = httpRequest(GET, "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))

			"handle simple request" in {
				val httpServletResponse = call(request)
				assert(httpServletResponse.text === "<!DOCTYPE html>\n<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}

		"recieving a HTTP GET request to a static resource on the file system" should {
			"serve valid requests" in {
				val httpServletResponse = call(httpRequest(GET, "resources" :: "static.html" :: Nil))
				assert(httpServletResponse.text === "static")
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}

			"handle not found (404)" in {
				val httpServletResponse = call(httpRequest(GET, "resources" :: "notFound.html" :: Nil))
				val expectedContent = NotFoundDefaultResponse.content.futureValue
				assert(httpServletResponse.text === "<!DOCTYPE html>\n" + expectedContent)
				assert(httpServletResponse.status === NotFound.code)
				assert(httpServletResponse.contentType === "text/html")
			}

			"handle it asyncrounously" in {
				val request = httpRequest(GET, "resources" :: "static.html" :: Nil)
				for (i <- 0 to 10) {
					val response = call(request)
					assert(response.text === "static")
				}
			}
		}
		"recieving a HTTP POST request to an in memory response (Hello World time)" should {
			val request = httpRequest(POST, "hello" :: "world" :: Nil, Map("parameter" -> Array("postedValue")))

			"handle simple request" in {
				val httpServletResponse = call(request)
				assert(httpServletResponse.text === "<!DOCTYPE html>\n<html><body>hello world postedValue</body></html>")
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.contentType === "text/html")
			}
		}

		"handle stateful response" should {
			"handle POST REDIRECT GET" in {
				val httpServletResponse = call(httpRequest(GET, "statefull" :: "request" :: Nil))
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val callback = httpServletResponse.text.replaceFirst("<!DOCTYPE html>\n", "").split("_").toList
				println("CALLBACK:" + callback)
				val cookies = httpServletResponse.cookies.toList

				val httpServletResponseStatefull = call(httpRequest(POST, callback, Map("key" -> Array("value")), cookies))
				assert(httpServletResponseStatefull.status === Found.code)
				val location = httpServletResponseStatefull.headers(Header.Location.name)
				assert(location.startsWith(List("some", "where", "new").mkString("/", "/", "")))

				val param = location.split("\\?")(1).split("=")
				val httpServletResponseRedirect = call(httpRequest(GET, List("some", "where", "new"), Map(param(0) -> param.tail), cookies))
				assert(httpServletResponseRedirect.status === OK.code)
				assert(httpServletResponseRedirect.contentType === "text/html")
				assert(httpServletResponseRedirect.text === "<!DOCTYPE html>\n<html><body>redirect post</body></html>")
			}
			"handle simple ajax post" in {
				val httpServletResponse = call(httpRequest(GET, "statefull" :: "ajaxrequest" :: Nil))
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.replaceFirst("<!DOCTYPE html>\n", "").split("_")
				val statefull = text(0)
				val id = text(1)
				val cookies = httpServletResponse.cookies.toList

				val httpServletResponseAjax = call(httpRequest(POST, "ajax" :: statefull :: id :: Nil, Map("key" -> Array("value")), cookies))
				assert(httpServletResponseAjax.text === "callback();")
				assert(httpServletResponseAjax.status === OK.code)
				assert(httpServletResponseAjax.contentType === "text/javascript")
			}

			"handle simple ajax callback" in {
				val request = httpRequest(GET, "statefull" :: "ajaxcallback" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.replaceFirst("<!DOCTYPE html>\n", "").split("_")
				val statefull = text(0)
				val key = text(1)
				val cookies = httpServletResponse.cookies.toList

				val httpServletResponseAjax = call(httpRequest(POST, "ajax" :: statefull :: Nil, Map(key -> Array("inputValue")), cookies))
				assert(httpServletResponseAjax.status === OK.code)
				assert(httpServletResponseAjax.contentType === "text/javascript")
				assert(httpServletResponseAjax.text === "inputCallback:inputValue();")
			}
			"handle simple ajax callback handler" in {
				val request = httpRequest(GET, "statefull" :: "ajaxhandler" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val text = httpServletResponse.text.replaceFirst("<!DOCTYPE html>\n", "").split("_")
				val statefull = text(0)
				val key = text(1)
				val cookies = httpServletResponse.cookies.toList

				val httpServletResponseAjax = call(httpRequest(POST, "ajax" :: statefull :: key :: Nil, Map(key -> Array("inputValue")), cookies))
				assert(httpServletResponseAjax.status === OK.code)
				assert(httpServletResponseAjax.contentType === "text/html")
				assert(httpServletResponseAjax.text === "<!DOCTYPE html>\ngot back here")
			}
		}

		"handle polling" should {
			"handle pushing messages" in {
				val request = httpRequest(GET, "statefull" :: "pull" :: Nil)
				val httpServletResponse = call(request)
				assert(httpServletResponse.status === OK.code)
				assert(httpServletResponse.contentType === "text/html")
				val statefull = httpServletResponse.text.replaceFirst("<!DOCTYPE html>\n", "")

				val httpServletResponsePull = call(httpRequest(POST, "pull" :: statefull :: Nil, cookies = httpServletResponse.cookies.toList))
				assert(httpServletResponsePull.status === OK.code)
				assert(httpServletResponsePull.contentType === "application/json")
				val RegExp = """\{"messages":\[\{"id":"(.*)", "message":"Message\(\);"\}\]\}""".r
				httpServletResponsePull.text match {
					case RegExp(id) =>
					case _ => assert(false, httpServletResponsePull.text + " --- " + RegExp)
				}
			}
		}
	}
}