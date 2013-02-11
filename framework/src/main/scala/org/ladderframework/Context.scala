package org.ladderframework

import java.io.InputStream
import java.util.Scanner
import java.util.UUID
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.annotation.implicitNotFound
import org.ladderframework.js.JsCmd
import org.ladderframework.logging.Loggable
import bootstrap.LadderBoot
import scala.concurrent.Future

object Context{
	private val lineSeparator = System.getProperty("line.separator")
	def createUUID:String = UUID.randomUUID.toString
	
	implicit def stream2String(is: InputStream): String = {
		val scanner = new Scanner(is)
		val sb = new StringBuilder
		while (scanner.hasNext) {
			sb.append(scanner.nextLine)
			if (scanner.hasNext) sb.append(lineSeparator)
		}
		sb.toString
	}
	
	def booleanValue(value: String):Option[Boolean] = {
			value match {
				case "1" | "true" | "yes" | "TRUE" | "YES" | "on" | "ON" => Some(true)
				case "0" | "false" | "no" | "FALSE" | "NO" | "off" | "OFF" => Some(false)
				case _ => None
			}
	}
}

case class Context(
		val contextID: String, 
		addResponse: (List[String], HttpResponse) => String, 
		update: JsCmd => Unit) extends Loggable {
	
	type Params = HttpRequest
	
	import Context._
	
	private var inputMap = Map[String, String => Unit]()
	private var booleanInputMap = Map[String, Boolean => Unit]()
	private var clickMap = Map[String, () => Unit]()
	private var fileInputMap = Map[String, FileInfo => Unit]()
	private var postMap = Map[String, Params => Future[(List[String], HttpResponse)]]()
	
	private var ajaxInputMap = Map[String, String => Future[JsCmd]]()
	private var ajaxBooleanInputMap = Map[String, Boolean => Future[JsCmd]]()
	private var ajaxClickMap = Map[String, () => Future[JsCmd]]()
	//private var ajaxFileInputMap = Map[String, FileInfo => JsCmd]()
	private var ajaxPostMap = Map[String, Params => Future[JsCmd]]()
	
	private var ajaxHandlerMap = Map[String, PartialFunction[HttpRequest, Future[HttpResponse]]]()

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

	def addInputValueCallback(callback: (String) => Unit): String = {
		debug("addInputValueCallback: " + callback)
		if(callback == null) throw new IllegalArgumentException("InputCallback sould not be null")
		addCallback(uuid => inputMap += (uuid -> callback))
	}

	def getInput(key: String): Option[String => Unit] = inputMap.get(key)

	def addSubmitCallback(callback: Params => Future[(List[String], HttpResponse)]): List[String] = {
		val funcUuid = createUUID
		postMap += (funcUuid -> callback)
		"post" :: contextID :: funcUuid :: Nil
	}
	
	def submitCallback:PartialFunction[HttpRequest, Future[HttpResponse]] = {
		case request @ HttpRequest(POST, _, "post" :: `contextID` :: func :: Nil, params, parts) if postMap.contains(func) =>
			debug("handleContextPost")
			debug("func: " + func + " --- " + postMap.get(func))
			params.foreach(param => {
				debug("param: " + param)
				val (name, value) = param
				inputMap.get(name).foreach(cb => value.foreach(cb))
				for{
					v <- value
					booleanValue <- booleanValue(v)
					callback <- booleanInputMap.get(name)
				} yield callback(booleanValue)
			})
			parts.foreach(part => {
				val name = part.getName
				inputMap.get(name).foreach(_(stream2String(part.getInputStream)))
				fileInputMap.get(name).foreach(_({
					FileInfo(part)
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
			import LadderBoot.executionContext
			postMap(func).apply(request).map{case (nextPath, response) => {
				val uuid = addResponse(nextPath, response)
				HttpRedirectResponse(nextPath, Option(uuid))
			}}
	}
	
	implicit def stream2String(is: InputStream): String = {
		val scanner = new Scanner(is)
		val sb = new StringBuilder
		while (scanner.hasNext) {
			sb.append(scanner.nextLine)
			if (scanner.hasNext) sb.append(lineSeparator)
		}
		sb.toString
	}
	
	private def addAjax(addFunc: String => Unit): List[String] = {
		val uuid = createUUID
		addFunc(uuid)
		"ajax" :: contextID :: uuid :: Nil
	}

	def addAjaxFormSubmitCallback(callback: Params => Future[JsCmd]): String = 
		addAjax(uuid => ajaxPostMap += (uuid -> callback)).mkString("/")

		
	case class AjaxCallbackCmd (lookupPath: List[String]) {
		val name = lookupPath.last
		val toCmd = "javascript:ladder.ajax('" + lookupPath.take(2).mkString("/", "/", "") + "', '" + name + "', event)"; 
	}
	
	def addAjaxBooleanCallback(callback: Boolean => Future[JsCmd]): AjaxCallbackCmd = {
		AjaxCallbackCmd( addAjax(uuid => ajaxBooleanInputMap += (uuid -> callback)))
	}

	def addAjaxClickCallback(callback: () => Future[JsCmd]): AjaxCallbackCmd = 
		AjaxCallbackCmd( addAjax(uuid => ajaxClickMap += (uuid -> callback)) )
	
	def addAjaxInputCallback(callback: String => Future[JsCmd]): AjaxCallbackCmd = 
		AjaxCallbackCmd(addAjax(uuid => ajaxInputMap += (uuid -> callback)))
	
	def addAjaxHandlerCallback(callback: PartialFunction[HttpRequest, Future[HttpResponse]]) : AjaxCallbackCmd = {
		AjaxCallbackCmd(addAjax(uuid => ajaxHandlerMap += (uuid -> callback)))
	}
	
	def ajaxCallback:PartialFunction[HttpRequest, Future[HttpResponse]] = {
		case HttpRequest(_, _, "ajax" :: `contextID` :: Nil, params, parts) => 
			debug("ajax callback ") 
			
			val jsCmd:Option[Future[JsCmd]] = params.headOption.flatMap(param => {
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
			import LadderBoot.executionContext
			jsCmd.map(_.map(msg => JsCmdResponse(msg))).getOrElse(Future(LadderBoot.notFound)) 
	}
	
	def ajaxSubmitCallback:PartialFunction[HttpRequest, Future[HttpResponse]] = {
		case request @ HttpRequest(_, _, "ajax" :: `contextID` :: func :: Nil, params, parts) if ajaxPostMap.contains(func) => 
			debug("ajax func: " + func + " --- " + ajaxPostMap(func))
			
			params.foreach(param => {
				val (key, values: Array[String]) = param
				for{
					callbackFunc <- inputMap.get(key)
					nullableValue <- values
					value <- Option(nullableValue)
				} {
					debug("key: " + key + "callbackFunc: " + callbackFunc + " --- value: " + value)
					callbackFunc(value)
				}
				
				for{
					v <- values
					booleanValue <- booleanValue(v)
					callback <- booleanInputMap.get(key)
				} yield callback(booleanValue)
				
			})
			params.foreach(param => {
				val (key, value) = param
				clickMap.get(key).foreach(_.apply())
			})
			val jsCmd: Future[JsCmd] = ajaxPostMap(func).apply(request)
			debug("result: " + jsCmd)
			import LadderBoot.executionContext
			jsCmd.map(JsCmdResponse)
		case request @ HttpRequest(_, _, "ajax" :: `contextID` :: func :: Nil, params, parts) if ajaxClickMap.contains(func) =>
			debug("ajax click func: " + func + " --- " + ajaxClickMap(func)) 
			
			import LadderBoot.executionContext
			ajaxClickMap(func).apply().map(JsCmdResponse)
	}
	
	private def notFound: PartialFunction[HttpRequest, Future[HttpResponse]] = {
		case _ =>
			import LadderBoot.executionContext
			Future(LadderBoot.notFound)
	}
	
	def ajaxHandlerCallback:PartialFunction[HttpRequest, Future[HttpResponse]] = {
		case req @ HttpRequest(_, _, "ajax" :: `contextID` :: func :: Nil, params, parts) if ajaxHandlerMap.contains(func) =>
			( ajaxHandlerMap(func) orElse notFound ).apply(req)
	}
}


