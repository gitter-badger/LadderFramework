package org.ladderframework.test.page

import org.ladderframework.test.response.IndexResponse
import org.ladderframework.Context
import scala.xml.Node
import org.ladderframework.test.response.AboutResponse
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise

case class AboutPageObject(response: AboutResponse)(implicit val context: Context, val executionContext: ExecutionContext) extends PageObject {
	
	val header: Promise[Node] = Promise().completeWith(resposeXml.future.map(xml => (xml \\ "h1").head))
	
}