package org.ladderframework.test.page

import org.ladderframework.test.response.IndexResponse
import org.ladderframework.Context
import scala.xml.Node
import org.ladderframework.test.response.AboutResponse

case class AboutPageObject(response: AboutResponse)(implicit val context: Context) extends PageObject {
	
	def header: Node = (resposeXml \\ "h1").head
	
}