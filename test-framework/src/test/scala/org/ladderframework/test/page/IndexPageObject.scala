package org.ladderframework.test.page

import org.ladderframework.test.response.IndexResponse
import org.ladderframework.Context
import scala.xml.Node
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext

case class IndexPageObject(response: IndexResponse)(implicit val context: Context, val executionContext: ExecutionContext) extends PageObject {
	
	val header: Promise[Node] = Promise().completeWith(resposeXml.future.map(xml => (xml \\ "h1").head))
	
	//def aboutHeader = Link[AboutPageObject]()
	
}