package org.ladderframework.test.response

import org.ladderframework.HtmlPage
import scala.xml.NodeSeq
import org.ladderframework.Context

class IndexResponse extends HtmlPage{
	val source = "index.html"
		
	def render(ns: NodeSeq)(implicit context: Context): NodeSeq = ns

}