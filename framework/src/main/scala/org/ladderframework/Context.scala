package org.ladderframework

import org.ladderframework.logging.Loggable
import java.util.UUID
import java.io.InputStream
import java.util.Scanner
import bootstrap.LadderBoot
import akka.actor.ActorRef
import org.ladderframework.js.JsCmd

object Context{
	private val lineSeparator = System.getProperty("line.separator")
	private val FileName = """.*filename="(.+)"""".r
}

case class Context(
		val contextID: String, 
		addResponse: (List[String], HttpResponse) => String, 
		update: String => Unit) extends Loggable {
	
	import Context._
	private var inputMap = Map[String, String => Unit]()
	private var booleanInputMap = Map[String, Boolean => Unit]()
	private var clickMap = Map[String, () => Unit]()
	private var fileInputMap = Map[String, FileInfo => Unit]()
	private var postMap = Map[String, () => (List[String], HttpResponse)]()
	
	private var ajaxInputMap = Map[String, String => JsCmd]()
	private var ajaxBooleanInputMap = Map[String, Boolean => JsCmd]()
	private var ajaxClickMap = Map[String, () => JsCmd]()
	//private var ajaxFileInputMap = Map[String, FileInfo => JsCmd]()
	private var ajaxPostMap = Map[String, () => JsCmd]()
	
	private var ajaxHandlerMap = Map[String, PartialFunction[HttpRequest, HttpResponse]]()

	private def createUUID:String = UUID.randomUUID.toString

	private def addCallback(addFunc: String => Unit):String = {
		val uuid = createUUID
		addFunc(uuid)
		uuid
	}
	
	def addClickCallback(callback: () => Unit): String = 
		addCallback(uuid => clickMap += (uuid -> callback))

	def getClickCallback(key: String): Option[() => Unit] = clickMap.get(key)

	def addInputBooleanCallback(callback: Boolean => Unit): String =
		addCallback(uuid => booleanInputMap += (uuid -> callback))

	def addFileCallback(callback: FileInfo => Unit): String = 
		addCallback(uuid => fileInputMap += (uuid -> callback))

	def getFileInputCallback(key: String): Option[FileInfo => Unit] = fileInputMap.get(key)

	def addInputValueCallback(callback: (String) => Unit): String = 
		addCallback(uuid => inputMap += (uuid -> callback))

	def getInput(key: String): Option[String => Unit] = inputMap.get(key)

	def addSubmitCallback(callback: () => (List[String], HttpResponse)): List[String] = {
		val funcUuid = createUUID
		postMap += (funcUuid -> callback)
		"post" :: contextID :: funcUuid :: Nil
	}
	
	private def booleanValue(value: String):Option[Boolean] = {
			value match {
				case "1" | "true" | "yes" | "TRUE" | "YES" => Some(true)
				case "0" | "false" | "no" | "FALSE" | "NO" => Some(false)
				case _ => None
			}
	}

	def submitCallback:PartialFunction[HttpRequest, HttpResponse] = {
		case HttpRequest(POST, _, "post" :: `contextID` :: func :: Nil, params, parts) if postMap.contains(func) => 
			debug("handleContextPost")
			debug("func: " + func + " --- " + postMap.get(func))
			params.foreach(param => {
				debug("param: " + param)
				val (key, value) = param
				inputMap.get(key).foreach(cb => value.foreach(cb))
				for{
					v <- value
					booleanValue <- booleanValue(v)
					callback <- booleanInputMap.get(key)
				} yield callback(booleanValue)
			})
			parts.foreach(part => {
				val name = part.getName
				inputMap.get(name).foreach(_(part.getInputStream))
				fileInputMap.get(name).foreach(_({
					val contentDisp = part.getHeader("content-disposition")
					val fileName = contentDisp match {
						case FileName(fn) => fn
						case _ => ""
					}
					FileInfo(fileName, part.getSize, part.getInputStream)
				}))
				clickMap.get(name).foreach(_())
			})
			params.foreach(param => {
				val (key, value) = param
				clickMap.get(key).foreach(_.apply())
			})
			parts.foreach(part => {
				val name = part.getName
				clickMap.get(name).foreach(_())
			})
			val (nextPath, response) = postMap(func).apply()
			val uuid = addResponse(nextPath, response)
			HttpRedirectResponse(nextPath, Option(uuid))
	}
	
	implicit private def stream2String(is: InputStream): String = {
		val scanner = new Scanner(is)
		val sb = new StringBuilder
		while (scanner.hasNext) {
			sb.append(scanner.nextLine)
			if (scanner.hasNext) sb.append(lineSeparator)
		}
		sb.toString
	}
	
	private def addAjax(addFunc: String => Unit):List[String] = {
		val uuid = createUUID
		addFunc(uuid)
		"ajax" :: contextID :: uuid :: Nil
	}

	def addAjaxSubmitCallback(callback: () => JsCmd): List[String] = 
		addAjax(uuid => ajaxPostMap += (uuid -> callback))

	def addAjaxBooleanCallback(callback: Boolean => JsCmd): List[String] = 
		addAjax(uuid => ajaxBooleanInputMap += (uuid -> callback))

	def addAjaxClickCallback(callback: () => JsCmd): List[String] = 
		addAjax(uuid => ajaxClickMap += (uuid -> callback))
	
	def addAjaxInputCallback(callback: String => JsCmd): List[String] = 
		addAjax(uuid => ajaxInputMap += (uuid -> callback))
	
	def addAjaxHandlerCallback(callback: PartialFunction[HttpRequest, HttpResponse]) : List[String] = {
		addAjax(uuid => ajaxHandlerMap += (uuid -> callback))
	}
	
	def ajaxCallback:PartialFunction[HttpRequest, HttpResponse] = {
		case HttpRequest(_, _, "ajax" :: `contextID` :: Nil, params, parts) => 
			debug("ajax callback ") 
			
			val jsCmd:Option[JsCmd] = params.headOption.flatMap(param => {
				val (key, values) = param
				val value = values.headOption.getOrElse("")
				ajaxInputMap.get(key).map(_(value)) orElse {
					for{
						booleanValue <- booleanValue(value)
						callback <- ajaxBooleanInputMap.get(key)
					} yield callback(booleanValue)
				} orElse {					
					ajaxClickMap.get(key).map(_.apply())
				}
			})
			jsCmd.map(_.toCmd).map(msg => JsCmdResponse(msg)).getOrElse(LadderBoot.notFound) 
	}
	
	def ajaxSubmitCallback:PartialFunction[HttpRequest, HttpResponse] = {
		case HttpRequest(_, _, "ajax" :: `contextID` :: func :: Nil, params, parts) if ajaxPostMap.contains(func) => 
			debug("ajax func: " + func + " --- " + ajaxPostMap(func)) 
			
			params.foreach(param => {
				val (key, value) = param
				inputMap.get(key).foreach(cb => value.foreach(cb))
				for{
					v <- value
					booleanValue <- booleanValue(v)
					callback <- booleanInputMap.get(key)
				} yield callback(booleanValue)
				
			})
			params.foreach(param => {
				val (key, value) = param
				clickMap.get(key).foreach(_.apply())
			})
			val jsCmd:String = ajaxPostMap(func).apply().toCmd
			JsCmdResponse(jsCmd)
	}
	
	private def notFound: PartialFunction[HttpRequest, HttpResponse] = {
		case _ => LadderBoot.notFound
	}
	
	def ajaxHandlerCallback:PartialFunction[HttpRequest, HttpResponse] = {
		case req @ HttpRequest(_, _, "ajax" :: `contextID` :: func :: Nil, params, parts) if ajaxHandlerMap.contains(func) =>
			( ajaxHandlerMap(func) orElse notFound ).apply(req)
	}
}


