package org.ladderframework

import scala.xml.NodeSeq
import scala.xml.XML
import javax.servlet.http.HttpServletResponse
import bootstrap.LadderBoot

trait StatelessHtmlPage extends HtmlResponse{
	def source: String
	
	final lazy val content = "<!DOCTYPE html>\n" + (render(xml)).toString
	
	override def applyToHttpServletResponse(httpServletResponse: HttpServletResponse)(implicit context: Context) {
		httpServletResponse.setStatus(status.code)
		httpServletResponse.setContentType(contentType)
		httpServletResponse.getWriter().append(content).flush()
	}
	
	private lazy val xml: NodeSeq = XML.load{LadderBoot.resource(source)}
	
	def render(ns: NodeSeq): NodeSeq

}
