package org.ladderframework.html.form

import scala.xml._
import org.ladderframework.Context
import org.ladderframework.js.JsCmd
import org.ladderframework.HttpResponse

trait Form extends Element {
	val tagName = "form"
	val children:NodeSeq => NodeSeq
	
	override def transform = (ns:NodeSeq) => {
		val Tag = tagName
		def tf(n: Node): Seq[Node] = {
			n match {
		    case Elem(prefix, Tag, attribs, scope, ch @ _*)  => 
		    	val newAttribs = handleAttributes(attribs) 
		      Elem(prefix, Tag, newAttribs, scope, children(ch):_*)
		    case other => other
		  }
		}
		ns.flatMap(tf)
	}
}

case class PostForm(
		submitCallback:() => (List[String], HttpResponse), 
		children:NodeSeq => NodeSeq)(
				implicit val context: Context) extends Form{
	
	override val overrideAttributes = Map[String, String]("action" -> context.addSubmitCallback(submitCallback).mkString("/"), 
			"method" -> "post")
	
}

case class AjaxForm(submitCallback:() => JsCmd, children:NodeSeq => NodeSeq)(
		implicit val context: Context) extends Form{
	
	override val overrideAttributes = Map[String, String]("action" -> context.addAjaxSubmitCallback(submitCallback).mkString("/"),
		"method" -> "ajax",
		"onsubmit" -> "isituptodate.submitFunction(event);",
		"onclick" -> "isituptodate.clickedLast(this, event)"
	)
}

