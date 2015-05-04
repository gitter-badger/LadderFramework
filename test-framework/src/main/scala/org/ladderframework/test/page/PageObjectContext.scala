package org.ladderframework.test.page

import org.ladderframework.Context
import org.ladderframework.StatefulHtmlResponse
import scala.xml.XML
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq
import scala.concurrent.Promise

case class PageObjectContext(response: StatefulHtmlResponse)(implicit val context:Context, val ec:ExecutionContext) {

	val responseXml:Promise[NodeSeq] = Promise[NodeSeq]()
	responseXml.completeWith(response.statefullContent)
	
}