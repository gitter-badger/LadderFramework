package org.ladderframework.test.page

import org.ladderframework.Context
import org.ladderframework.StatefulHtmlResponse
import scala.xml.XML
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq
import scala.concurrent.Promise

trait PageObject {
	implicit def context: Context
	implicit def executionContext: ExecutionContext
	def response: StatefulHtmlResponse
	
	implicit lazy val pageObjectContext = PageObjectContext(response)
	val resposeXml:Promise[NodeSeq] = Promise().completeWith(pageObjectContext.responseXml.future)
	
}