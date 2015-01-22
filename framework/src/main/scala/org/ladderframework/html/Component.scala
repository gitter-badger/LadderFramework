package org.ladderframework.html

import scala.xml.NodeSeq
import scala.xml.XML
import org.ladderframework.css.CssSelector
import org.ladderframework.css.CssSelector._
import org.ladderframework.Utils
import scala.concurrent._
import org.ladderframework.Context

trait Component {
	def source:String
	def context: Context
	implicit def executionContext: ExecutionContext
	
	lazy val xml:Promise[NodeSeq] = Promise[NodeSeq].completeWith(Future{
		val resouce = context.boot.resource(source)
		XML.load(resouce)
	})
	val extract:CssSelector
	
	def render:Future[NodeSeq => NodeSeq]
	
	def content:Future[NodeSeq] = render.flatMap(renderFunc => xml.future.map(_.extract(extract).head).map(renderFunc))

}

object Component{
	
	def apply(source:String, extract:CssSelector, render:Future[NodeSeq => NodeSeq])(implicit context: Context, ec: ExecutionContext):Component = {
		val s = source
		val r = render
		val e = extract
		val c = context
		new Component{
			final override val source = s
			final override val extract = e
			final override val executionContext = ec
			val context = c
			override def render = r
		}
	}
	
	implicit def componentToNodeSeq[C <: Component](c:C):Future[NodeSeq] = c.content
}

