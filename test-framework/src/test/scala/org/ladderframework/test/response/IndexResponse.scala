package org.ladderframework.test.response

import org.ladderframework.HtmlPage
import scala.xml.NodeSeq
import org.ladderframework.Context
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class IndexResponse extends HtmlPage{
	val source = "index.html"
		
	override def render(implicit context: Context, ec: ExecutionContext): Future[NodeSeq => NodeSeq] = Future(ns => ns)

}