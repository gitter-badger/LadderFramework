package org.ladderframework

import scala.xml.NodeSeq
import scala.xml.XML
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import org.ladderframework.exception.RetrievingXMLException

trait StatelessHtmlPage extends HtmlResponse{
	def source: String
	def boot: DefaultBoot
	implicit def ec: ExecutionContext
	
	override final def content():Future[NodeSeq] = {
		for{
			x <- xml
			r <- render
		} yield (r(x))
	}
	
	private def xml():Future[NodeSeq] = Future{
		try{
			XML.load(boot.resource(source))
		}catch{
			case t:Throwable => throw new RetrievingXMLException("Error loading xml: " + source, t)
		}
	}
	
	def render():Future[NodeSeq => NodeSeq]

}
