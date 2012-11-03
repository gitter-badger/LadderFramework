package org.ladderframework

import scala.xml.NodeSeq
import scala.xml.XML
import javax.servlet.http.HttpServletResponse
import bootstrap.LadderBoot
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise

trait StatelessHtmlPage extends HtmlResponse{
	def source: String
	
	override final def content(implicit ec:ExecutionContext):Future[String] = {
		for{
			x <- xml
			r <- render
		} yield "<!DOCTYPE html>\n" + (r(x)).toString
	}
	
	override def applyToHttpServletResponse(httpServletResponse: HttpServletResponse)(implicit context: Context, ec:ExecutionContext) =
		content.map(cont => {
			httpServletResponse.setStatus(status.code)
			httpServletResponse.setContentType(contentType)
			httpServletResponse.getWriter().append(cont).flush()
			status
		})

	
	private def xml(implicit ec:ExecutionContext):Future[NodeSeq] = Future{
		XML.load{LadderBoot.resource(source)}
	}
	
	def render(implicit ec:ExecutionContext):Future[NodeSeq => NodeSeq]

}
