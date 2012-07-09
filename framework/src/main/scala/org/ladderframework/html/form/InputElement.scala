package org.ladderframework.html.form

import java.io.InputStream
import scala.xml._
import org.ladderframework.Context
import org.ladderframework.FileInfo
import org.ladderframework.validation.Validation
import org.ladderframework.validation.ValidationError

trait InputElement[T] extends Element with Validation[T]{
	
	override val tagName = "input"
	val callback: T => Unit 
	
}

trait InputValueElement[T] extends InputElement[T] {
	val value: String
}

trait InputOnlyElement[T] extends InputElement[T]

case class TextInput(value: String, callback: String => Unit)(implicit val context: Context) extends InputValueElement[String]{
	override val overrideAttributes = Map("name" -> context.addInputValueCallback(callback),
		"type" -> "text",
		"value" -> value)
}

case class PasswordInput(value: String, callback: String => Unit)(implicit val context: Context) extends InputValueElement[String]{
	override val overrideAttributes = Map("name" -> context.addInputValueCallback(callback),
		"type" -> "password",
		"value" -> value)
}

case class FileInput(callback: FileInfo => Unit)(implicit val context: Context) extends InputOnlyElement[FileInfo]{
	override val overrideAttributes = Map("name" -> context.addFileCallback(callback),
		"type" -> "file")
}

case class CheckboxInput(value: Boolean, callback: Boolean => Unit)(implicit val context: Context) extends InputElement[Boolean]{
	override val overrideAttributes = Map("name" -> context.addInputBooleanCallback(callback),
		"type" -> "checkbox")
		//remove("checked") 
}

trait ClickElement extends Element{
	implicit val context: Context
	
	val callback: () => Unit
	override val overrideAttributes = Map("name" -> context.addClickCallback(callback))
}

case class Button(callback: () => Unit)(implicit val context: Context) extends ClickElement{
	val tagName = "button"
}

case class InputButton(callback: () => Unit)(implicit val context: Context) extends ClickElement{
	val tagName = "input"
		
	override val overrideAttributes = Map("name" -> context.addClickCallback(callback), "type" -> "button")
}

case class SubmitButton(callback: () => Unit)(implicit val context: Context) extends ClickElement{
	val tagName = "input"
		
	override val overrideAttributes = Map("name" -> context.addClickCallback(callback), "type" -> "submit")
}


//TODO

//checkbox
//hidden
//image
//radio
//reset - ?

//class RadioButton(val callback: () => Unit) extends ClickElement

//class RadioGroup()

//class Select[T](items: Seq[T], toText: T=>String, value: Option[T], callback: T => Unit)

//class MultiSelect[T](items: Seq[T], toText: T=>String, value: Seq[T], callback: Seq[T] => Unit)

case class Textarea(value: String, callback: String => Unit)(implicit val context: Context) extends InputValueElement[String]{
	override val tagName = "textarea"
		
	override def transform = (ns: NodeSeq) => {
		val Tag = tagName
		def tf(n: Node): Seq[Node] = n match {
	    case Elem(prefix, Tag, attribs, scope, children @ _*)  => 
	    	val newAttribs = attribs remove("name") append Attribute(None, "name", Text(context.addInputValueCallback(callback)), Null)
	      Elem(prefix, Tag, newAttribs, scope, Text(value))
	    case other => other
	  }
		ns.flatMap(tf)
	}
}
