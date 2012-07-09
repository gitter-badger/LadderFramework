package org.ladderframework.test.page

import org.ladderframework.Context
import org.ladderframework.StatefulHtmlResponse
import scala.xml.XML

case class PageObjectContext(response: StatefulHtmlResponse)(implicit val context:Context) {

	lazy val resposeXml = XML.loadString(response.statefullContent)
	
}