package org.ladderframework.test.html.form

import java.io.File
import java.io.FileInputStream

import scala.Option.option2Iterable
import scala.collection.immutable.HashMap
import scala.xml.NodeSeq.seqToNodeSeq

import org.ladderframework.css.CssSelector
import org.ladderframework.css.CssSelector._
import org.ladderframework.FileInfo
import org.ladderframework.StatefulHtmlResponse
import org.ladderframework.HttpRequest
import org.ladderframework.HttpResponse
import org.ladderframework.POST
import org.ladderframework.Context
import org.ladderframework.test.page.PageObjectContext

trait Element{
	implicit val pageObjectContext:PageObjectContext
	val selector:CssSelector
}

trait InputValueElement[T] extends Element{
	def input(value:T){
		val node = pageObjectContext.resposeXml.extract(selector).head
		val name = (node \ "@name").text
		pageObjectContext.response match {
			case cr: StatefulHtmlResponse =>
				val context = pageObjectContext.context
				applyCallback(context, name, value)
			case _ => 
		}
	}
	
	def applyCallback(context: Context, name: String, value: T)
	
}

class TextInput(implicit val pageObjectContext: PageObjectContext) extends InputValueElement[String]{
	val selector:CssSelector = "input[type=text]"
		
	override def applyCallback(context: Context, name:String, value:String) {
		context.getInput(name).foreach(_.apply(value))
	}
}

class FileInput(implicit val pageObjectContext: PageObjectContext) extends InputValueElement[File]{
	val selector:CssSelector = "input[type=file]"
		
	override def applyCallback(context: Context, name:String, value:File) {
		val fileInfo = FileInfo(
				name = value.getName, 
				size = value.length, 
				inputStream = new FileInputStream(value))
		context.getFileInputCallback(name).foreach(_.apply(fileInfo))
	}
}


trait ClickElement extends Element

case class Button(implicit val pageObjectContext: PageObjectContext) extends ClickElement{
	val selector:CssSelector = "button"
}

case class InputButton(implicit val pageObjectContext: PageObjectContext) extends ClickElement{
	val selector:CssSelector = "input[type=button]"
}

case class SubmitButton(implicit val pageObjectContext: PageObjectContext) extends ClickElement{
	val selector:CssSelector = "input[type=submit]"
	
	val sessionID = ""
		
	val context = pageObjectContext.context	
	
	def click[R<:HttpResponse](implicit manifest:Manifest[R]):Option[R] = {
		val nodes = pageObjectContext.resposeXml.extract(selector)
		val name = (nodes \ "@name").text
		context.getClickCallback(name).foreach(_.apply())
		//Submit form
		val forms = pageObjectContext.resposeXml.extract("form")
		forms.map(_ \ "@action").map(_.text).map(action => {
			context.submitCallback(HttpRequest(POST, sessionID, action.split("/").toList))
		}).headOption.collect{
			case x:Any if manifest.runtimeClass.isInstance(x) => x.asInstanceOf[R]
		}
	}
		
}