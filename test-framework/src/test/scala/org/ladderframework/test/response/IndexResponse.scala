package org.ladderframework.test.response

import org.ladderframework.StatefulHtmlPage
import scala.xml.NodeSeq
import org.ladderframework.Context
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class IndexResponse extends StatefulHtmlPage{
	val source = "index.html"
		
	override def render(implicit context: Context, ec: ExecutionContext): Future[NodeSeq => NodeSeq] = Future(ns => ns)

}