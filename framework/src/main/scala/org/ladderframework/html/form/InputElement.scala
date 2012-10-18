package org.ladderframework.html.form

import java.io.InputStream
import scala.xml._
import org.ladderframework.Context
import org.ladderframework.FileInfo
import org.ladderframework.HttpRequest
import org.ladderframework.js.JsCmd
import org.ladderframework.html.validation.InputValidation
import scalaz._
import Scalaz._
import org.ladderframework.html.validation.ValidationError

trait InputElement[Type, Raw] extends Element{
	override val tagName = "input"
	val callback: Type => Unit 
	def getParamter(in: HttpRequest): Option[Raw]
	val nameId: String
}

trait InputValueElement[Type] extends InputElement[Type, String] with InputValidation[Type] {
	val value: Type
	
}

trait StringInputValueElement[Type] extends InputValueElement[Type]{
	lazy val inputValueCallback: String => Unit = in => expectConvertValidate(Some(in)).foreach(callback)
	lazy val nameId = context.addInputValueCallback(inputValueCallback)
	
	implicit def valueToString(in:Type): String = in.toString 
	
	override def getParamter(in: HttpRequest):Option[String] = 
		in.parameters.get(nameId).flatMap(_.headOption).orElse(in.partAsString(nameId))
}

trait InputOnlyElement[Type] extends InputElement[Type, Type]

case class TextInput[T](
		value: T, 
		callback: T => Unit = (t:T) => {},
		expectConvertValidate: Option[String] => ValidationNEL[ValidationError, T]
		)(implicit val context: Context) extends StringInputValueElement[T]{
	
	override val overrideAttributes: Map[String, String] = Map("name" -> nameId,
		"type" -> "text",
		"value" -> value)
}

case class PasswordInput(value: String, 
		callback: String => Unit = _ => {},
		expectConvertValidate: Option[String] => ValidationNEL[ValidationError, String]
	)(implicit val context: Context) extends StringInputValueElement[String]{
	
	override val overrideAttributes = Map("name" -> nameId,
		"type" -> "password",
		"value" -> value)
}

case class FileInput(callback: FileInfo => Unit = _ => {})(implicit val context: Context) extends InputOnlyElement[FileInfo]{
	val nameId = context.addFileCallback(callback)
	
	override def getParamter(in: HttpRequest): Option[FileInfo] = in.part(nameId).map(FileInfo(_))
	
	override val overrideAttributes = Map("name" -> nameId,
		"type" -> "file")
}

case class CheckboxInput(value: Boolean, callback: Boolean => Unit = _ => {})(implicit val context: Context) extends InputElement[Boolean, String]{
	val nameId:String = context.addInputBooleanCallback(callback)
	override def getParamter(in: HttpRequest): Option[String] = 
			in.parameters.get(nameId).flatMap(_.headOption).orElse(in.partAsString(nameId))
	
	override val overrideAttributes = Map("name" -> nameId,
		"type" -> "checkbox")
		//remove("checked") 
}

trait ClickElement extends Element{
	implicit val context: Context
	val callback: () => Unit
	override val overrideAttributes = Map("name" -> context.addClickCallback(callback))
}

case class Button(callback: () => Unit = () => {})(implicit val context: Context) extends ClickElement{
	val tagName = "button"
}

case class InputButton(callback: () => Unit = () => {})(implicit val context: Context) extends ClickElement{
	val tagName = "input"
	override val overrideAttributes = Map("name" -> context.addClickCallback(callback), "type" -> "button")
}

case class SubmitButton(callback: () => Unit = () => {})(implicit val context: Context) extends ClickElement{
	val tagName = "input"
	override val overrideAttributes = Map("name" -> context.addClickCallback(callback), "type" -> "submit")
}

case class AjaxButton(callback: () => JsCmd)(implicit val context: Context) extends Element{
	val tagName = "input"
	override val overrideAttributes = { 
		val cmd = context.addAjaxClickCallback(callback)
		Map("onclick" -> cmd.toCmd, "type" -> "button", "name" -> cmd.name)
	}
}

//TODO

//hidden
//image
//radio
//reset - ?

//class RadioButton(val callback: () => Unit) extends ClickElement

//class RadioGroup()

//class Select[T](items: Seq[T], toText: T=>String, value: Option[T], callback: T => Unit)

//class MultiSelect[T](items: Seq[T], toText: T=>String, value: Seq[T], callback: Seq[T] => Unit)

case class Textarea(
		value: String, 
		callback: String => Unit = _ => {},
		expectConvertValidate: Option[String] => ValidationNEL[ValidationError, String]
		)(implicit val context: Context) extends StringInputValueElement[String]{
	override val tagName = "textarea"
		
	override def transform = (ns: NodeSeq) => {
		val Tag = tagName
		def tf(n: scala.xml.Node): NodeSeq = n match {
	    case Elem(prefix, Tag, attribs, scope, children @ _*)  => 
	    	val newAttribs = attribs remove("name") append Attribute(None, "name", Text(nameId), Null)
	      Elem(prefix, Tag, newAttribs, scope, Text(value))
	    case other => other
	  }
		ns.flatMap(tf)
	}
}

