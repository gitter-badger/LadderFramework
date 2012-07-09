package org.ladderframework.html

import scala.xml.NodeSeq
import scala.xml.XML
import org.ladderframework.css.CssSelector
import org.ladderframework.css.CssSelector._
import org.ladderframework.Utils
import bootstrap.LadderBoot

trait Component {
	val source:String
	
	lazy val xml:NodeSeq = {
		val resouce = LadderBoot.resource(source)
		XML.load(resouce)
	}
	val extract:CssSelector
	
	def render:NodeSeq => NodeSeq
	
	def content = {
		render(xml.extract(extract).head)
	}

}

object Component{
	
	def apply(source:String, extract:CssSelector, render:NodeSeq => NodeSeq):Component = {
		val s = source
		val r = render
		val e = extract
		new Component{
			final override val source = s
			final override val extract = e
			override def render = r
		}
	}
	
	implicit def componentToNodeSeq[C <: Component](c:C):NodeSeq = c.content
}

