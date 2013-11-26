package org.ladderframework

import scala.xml.NodeSeq
import scala.xml.XML
import java.io.InputStream
import bootstrap.LadderBoot
import java.io.File
import org.ladderframework.logging.Loggable
import org.ladderframework.css.CssSelector._
import org.ladderframework.js.JsCmd
import java.io.StringWriter
import java.io.PrintWriter
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise
import scala.xml.Xhtml

trait HttpResponse {
	def status: Status

	def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext): Future[Status]
}

case class HttpRedirectResponse(location: List[String], params: Option[String] = None) extends HttpResponse {
	val status = Found

	override def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext): Future[Status] = {
		Future {
			httpResponseOutput.setStatus(status)
			httpResponseOutput.setHeader(Location, location.mkString("/", "/", "") + params.map("?" + _ + "=redirect").getOrElse(""))
			status
		}
	}
}

object NotFoundResponse extends HtmlResponse {
	override final val status = NotFound
	override def content(implicit ec: ExecutionContext): Future[String] = Future.successful(
		<html><head><title>404</title></head><body>404</body></html>.toString)
}

case class ErrorResponse(override val status: Status, ot: Option[Throwable]) extends HtmlResponse {

	lazy val throwableString: String = {
		ot.map(t => {
			val sw = new StringWriter
			val pw = new PrintWriter(sw)
			t.printStackTrace(pw);
			sw.toString()
		}).getOrElse("")
	}

	def content(implicit ec: ExecutionContext): Future[String] = Future {
		<html>
			<head><title>{ status.code }</title></head>
			<body><h1>{ status.code }</h1><h2>{ ot.map(_.getMessage).getOrElse("") }</h2><pre>{ throwableString }</pre></body>
		</html>.toString
	}
}

case class HttpResourceResponse(status: Status = OK, path: List[String]) extends HttpResponse with Loggable {
	lazy val pathString = path.mkString("/", "/", "")

	override def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext): Future[Status] = Future {
		LadderBoot.resource(pathString)
	}.flatMap(file => {
		debug("HttpResourceResponse - print: " + file)
		if (file != null) {
			Future {
				httpResponseOutput.setStatus(status)
				path.reverse.headOption.foreach(contentType =>
					httpResponseOutput.setContentType(LadderBoot.mimeType(contentType)))
				val content = LadderBoot.resourceAsStream(pathString)
				val out = httpResponseOutput.outputStream
				Iterator.continually(content.read).takeWhile(-1 !=).foreach(out.write)
				status
			}
		} else {
			LadderBoot.notFound.applyToHttpServletResponse(httpResponseOutput)
		}
	})
}

case class XmlResponse(content: NodeSeq) extends HttpResponse {
	def status: Status = OK
	def contentType = "text/xml"
	override def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext) = Future {
		httpResponseOutput.setStatus(status)
		httpResponseOutput.setContentType(contentType)
		httpResponseOutput.writer.append(content.mkString).flush()
		status
	}
}

case class JsonResponse(content: String) extends HttpResponse {
	def status: Status = OK
	def contentType = "text/json"
	override def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext) = Future {
		httpResponseOutput.setStatus(status)
		httpResponseOutput.setContentType(contentType)
		httpResponseOutput.writer.append(content).flush()
		status
	}
}
case class JsCmdResponse(cmd: JsCmd) extends HttpResponse with Loggable {
	val status: Status = OK
	val contentType = "text/javascript"
	override def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext) = Future {
		debug(this)
		httpResponseOutput.setStatus(status)
		httpResponseOutput.setContentType(contentType)
		httpResponseOutput.writer.append(cmd.toCmd).flush()
		status
	}
}

case class PullResponse(messages: List[PushMessage]) extends HttpResponse with Loggable {
	val status: Status = OK
	val contentType = "text/json"
	override def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext) = Future {
		debug(this)
		httpResponseOutput.setStatus(status)
		httpResponseOutput.setContentType(contentType)
		httpResponseOutput.writer.append(messages.map(_.asJson).mkString("""{"messages":[""", ",", "]}")).flush()
		status
	}
}

trait HtmlResponse extends HttpResponse {
	def status: Status = OK
	def contentType = "text/html"
	def content(implicit ec: ExecutionContext): Future[String]
	override def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext) =
		content.map(cont => {
			httpResponseOutput.setStatus(status)
			httpResponseOutput.setContentType(contentType)
			httpResponseOutput.writer.append(cont).flush()
			status
		})
}

object HtmlResponse {

	def apply(html: String) = new HtmlResponse {
		def content(implicit ec: ExecutionContext) = Future.successful(html)
	}
}

trait Stateful {
	self: HtmlResponse =>
	def statefullContent(implicit context: Context, ec: ExecutionContext): Future[String]
	final override def content(implicit ec: ExecutionContext) = Future("")
}

trait StatefulHtmlResponse extends HtmlResponse with Stateful {
	override def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext) =
		statefullContent.map(content => {
			httpResponseOutput.setStatus(status)
			httpResponseOutput.setContentType(contentType)
			httpResponseOutput.writer.append(content).flush()
			status
		})
}

trait HtmlPage extends StatefulHtmlResponse with Loggable {
	val source: String

	private val xml: Promise[NodeSeq] = Promise()

	def addPush(implicit context: Context): NodeSeq => NodeSeq = {
		"body" #+> <script type="text/javascript">{ "$(function(){ladder.push('" + context.contextID + "');})" }</script>
	}

	def render(implicit context: Context, ec: ExecutionContext): Future[NodeSeq => NodeSeq]
	override def statefullContent(implicit context: Context, ec: ExecutionContext): Future[String] = {
		for {
			x <- xml.completeWith(Future {
				val resouce = LadderBoot.resource(source)
				XML.load(resouce)
			}).future
			r <- render
		} yield "<!DOCTYPE html>\n" + Xhtml.toXhtml(addPush.apply(r(x)))
	}
}

sealed abstract class Status(val code: Int)

object Continue extends Status(100)
object SwitchingProtocols extends Status(101)
object OK extends Status(200)
object Created extends Status(201)
object Accepted extends Status(202)
object NonAuthoritativeInformation extends Status(203)
object NoContent extends Status(204)
object ResetContent extends Status(205)
object PartialContent extends Status(206)
object MultipleChoices extends Status(300)
object MovedPermanently extends Status(301)
object Found extends Status(302)
object SeeOther extends Status(303)
object NotModified extends Status(304)
object UseProxy extends Status(305)
object TemporaryRedirect extends Status(307)
object BadRequest extends Status(400)
object Unauthorized extends Status(401)
object PaymentRequired extends Status(402)
object Forbidden extends Status(403)
object NotFound extends Status(404)
object MethodNotAllowed extends Status(405)
object NotAcceptable extends Status(406)
object ProxyAuthenticationRequired extends Status(407)
object RequestTimeOut extends Status(408)
object Conflict extends Status(409)
object Gone extends Status(410)
object LengthRequired extends Status(411)
object PreconditionFailed extends Status(412)
object RequestEntityTooLarge extends Status(413)
object RequestURITooLarge extends Status(414)
object UnsupportedMediaType extends Status(415)
object RequestedRangeNotSatisfiable extends Status(416)
object ExpectationFailed extends Status(417)
object InternalServerError extends Status(500)
object NotImplemented extends Status(501)
object BadGateway extends Status(502)
object ServiceUnavailable extends Status(503)
object GatewayTimeOout extends Status(504)
object HTTPVersionNotNupported extends Status(505)
