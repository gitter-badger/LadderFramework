package org.ladderframework

import scala.xml.NodeSeq
import scala.xml.XML
import java.io.InputStream
import javax.servlet.http.HttpServletResponse
import bootstrap.LadderBoot
import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import java.io.File
import org.ladderframework.logging.Loggable

trait HttpResponse{
	def status:Status 
	
	def applyToHttpServletResponse(httpServletResponse:HttpServletResponse)(implicit context:Context):Unit
}

case class HttpRedirectResponse(location:List[String], params: Option[String] = None) extends HttpResponse{
	val status = Found
	
	override def applyToHttpServletResponse(httpServletResponse:HttpServletResponse)(implicit context:Context):Unit = {
		httpServletResponse.setStatus(status.code)
		httpServletResponse.setHeader("Location", location.mkString("/", "/", "") + params.map("?" + _ + "=redirect").getOrElse(""))
	}
}

object NotFoundResponse extends HtmlResponse{
	override final val status = NotFound
	def content:String = <html><head><title>404</title></head><body>404</body></html>.toString
}

case class HttpResourceResponse(status:Status = OK, path:List[String]) extends HttpResponse with Loggable {
	
	override def applyToHttpServletResponse(httpServletResponse: HttpServletResponse)(implicit context:Context) {
		val file = LadderBoot.resource(path.mkString("/"))
		debug("HttpResourceResponse - print: " + file)
		if(file != null){
			httpServletResponse.setStatus(status.code)
			path.reverse.headOption.foreach(contentType => 
				httpServletResponse.setContentType(LadderBoot.mimeType(contentType))
			)
			val content = LadderBoot.resourceAsStream(path.mkString("/"))
			val out = httpServletResponse.getOutputStream()
	    Iterator.continually (content.read).takeWhile (-1 !=).foreach(out.write)
		}else{
			LadderBoot.notFound.applyToHttpServletResponse(httpServletResponse)
		}
	}
}

case class JsonResponse(content:String) extends HttpResponse{
	def status:Status = OK
	def contentType = "text/json"	
	override def applyToHttpServletResponse(httpServletResponse: HttpServletResponse)(implicit context:Context) {
		httpServletResponse.setStatus(status.code)
		httpServletResponse.setContentType(contentType)
		httpServletResponse.getWriter().append(content).flush()
	}
}
case class JsCmdResponse(content:String) extends HttpResponse with Loggable{
	val status:Status = OK
	val contentType = "text/javascript"	
	override def applyToHttpServletResponse(httpServletResponse: HttpServletResponse)(implicit context:Context) {
		debug(this)	
		httpServletResponse.setStatus(status.code)
		httpServletResponse.setContentType(contentType)
		httpServletResponse.getWriter().append(content).flush()
	}
}

case class PullResponse(messages:List[PushMessage]) extends HttpResponse with Loggable{
	val status:Status = OK
	val contentType = "text/json"	
	override def applyToHttpServletResponse(httpServletResponse: HttpServletResponse)(implicit context:Context) {
		debug(this)	
		httpServletResponse.setStatus(status.code)
		httpServletResponse.setContentType(contentType)
		httpServletResponse.getWriter().append(messages.map(_.asJson).mkString("[", ",", "]")).flush()
	}
}

trait HtmlResponse extends HttpResponse{
	def status:Status = OK
	def contentType = "text/html"	
	def content:String
	override def applyToHttpServletResponse(httpServletResponse: HttpServletResponse)(implicit context:Context) {
		httpServletResponse.setStatus(status.code)
		httpServletResponse.setContentType(contentType)
		httpServletResponse.getWriter().append(content).flush()
	}
}

object HtmlResponse{
	
	def apply(html: String) = new HtmlResponse{
		def content = html
	}
}

trait Stateful{
	self: HtmlResponse => 
	def statefullContent(implicit context:Context):String
	final def content = "" 
}

trait StatefulHtmlResponse extends HtmlResponse with Stateful{
	override def applyToHttpServletResponse(httpServletResponse: HttpServletResponse)(implicit context:Context) {
		httpServletResponse.setStatus(status.code)
		httpServletResponse.setContentType(contentType)
		httpServletResponse.getWriter().append(statefullContent).flush()
	}
}


trait HtmlPage extends StatefulHtmlResponse with Loggable{
	val source: String
	
	private lazy val xml: NodeSeq = {
		val resouce = LadderBoot.resource(source)
		XML.load(resouce)
	}

	def render(ns: NodeSeq)(implicit context:Context): NodeSeq
	def statefullContent(implicit context:Context):String = "<!DOCTYPE html>\n" + render(xml).toString

}

sealed abstract class Status(val code:Int)

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
