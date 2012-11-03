package org.ladderframework.test.response

import org.ladderframework.HtmlPage
import scala.xml.NodeSeq
import org.ladderframework.Context
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class AboutResponse extends HtmlPage{
	val source = "about.html"
		
	override def render(implicit context: Context, ec: ExecutionContext): Future[NodeSeq => NodeSeq] = Future(ns => ns)

}