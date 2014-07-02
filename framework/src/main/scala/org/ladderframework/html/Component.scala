package org.ladderframework.html

import scala.xml.NodeSeq
import scala.xml.XML
import org.ladderframework.css.CssSelector
import org.ladderframework.css.CssSelector._
import org.ladderframework.Utils
import bootstrap.LadderBoot
import scala.concurrent._
import bootstrap.LadderBoot.executionContext

trait Component {
	def source:String
	
	lazy val xml:Promise[NodeSeq] = Promise[NodeSeq].completeWith(Future{
		val resouce = LadderBoot.resource(source)
		XML.load(resouce)
	})
	val extract:CssSelector
	
	def render:Future[NodeSeq => NodeSeq]
	
	def content:Future[NodeSeq] = render.flatMap(renderFunc => xml.future.map(_.extract(extract).head).map(renderFunc))

}

object Component{
	
	def apply(source:String, extract:CssSelector, render:Future[NodeSeq => NodeSeq]):Component = {
		val s = source
		val r = render
		val e = extract
		new Component{
			final override val source = s
			final override val extract = e
			override def render = r
		}
	}
	
	implicit def componentToNodeSeq[C <: Component](c:C):Future[NodeSeq] = c.content
}

