package org.ladderframework.html.form

import scala.xml._
import org.ladderframework.Context

trait Element {
	final def handleAttributes(attributes: MetaData) = {
		overrideAttributes.foldLeft(attributes)((attrs, pair) => {
			val (key, value) = pair
			attrs remove(key) append Attribute(None, key, Text(value), Null)
		})
	}
	
	protected def overrideAttributes = Map[String, String]()
	
	val tagName:String
	val context: Context
	
	def transform = (ns: NodeSeq) => {
		val Tag = tagName
		def tf(n: Node): Seq[Node] = n match {
	    case Elem(prefix, Tag, attribs, scope, children @ _*)  => 
	    	val newAttribs = handleAttributes(attribs) 
	      Elem(prefix, Tag, newAttribs, scope, children:_*)
	    case other => other
	  }
		ns.flatMap(tf)
	}
}


case class Label(value: NodeSeq, forInputElement: InputValueElement[_])