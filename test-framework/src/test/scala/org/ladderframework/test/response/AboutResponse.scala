package org.ladderframework.test.response

import org.ladderframework.HtmlPage
import scala.xml.NodeSeq
import org.ladderframework.Context

class AboutResponse extends HtmlPage{
	val source = "about.html"
		
	def render(ns: NodeSeq)(implicit context: Context): NodeSeq = ns

}