package org.ladderframework.test.page

import org.ladderframework.Context
import org.ladderframework.StatefulHtmlResponse
import scala.xml.XML

trait PageObject {
	implicit val context: Context
	val response: StatefulHtmlResponse
	
	implicit lazy val pageObjectContext = PageObjectContext(response)
	lazy val resposeXml = pageObjectContext.resposeXml
	
}