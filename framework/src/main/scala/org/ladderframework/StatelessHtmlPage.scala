package org.ladderframework

import scala.xml.NodeSeq
import scala.xml.XML
import bootstrap.LadderBoot
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import org.ladderframework.exception.RetrievingXMLException

trait StatelessHtmlPage extends HtmlResponse{
	def source: String
	
	override final def content(implicit ec:ExecutionContext):Future[String] = {
		for{
			x <- xml
			r <- render
		} yield "<!DOCTYPE html>\n" + (r(x)).toString
	}
	
	private def xml(implicit ec:ExecutionContext):Future[NodeSeq] = Future{
		try{
			XML.load{LadderBoot.resource(source)}
		}catch{
			case t:Throwable => throw new RetrievingXMLException("Error loading xml: " + source, t)
		}
	}
	
	def render(implicit ec:ExecutionContext):Future[NodeSeq => NodeSeq]

}
