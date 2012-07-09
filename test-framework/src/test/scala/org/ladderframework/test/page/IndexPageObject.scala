package org.ladderframework.test.page

import org.ladderframework.test.response.IndexResponse
import org.ladderframework.Context
import scala.xml.Node

case class IndexPageObject(response: IndexResponse)(implicit val context: Context) extends PageObject {
	
	def header: Node = (resposeXml \\ "h1").head
	
	def aboutHeader = Link[AboutPageObject]()
	
}