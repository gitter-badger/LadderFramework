package org.ladderframework
import javax.servlet.http.Part

case class HttpRequest(
		method:Method,
		sessionID:String,
		path:List[String], 
		parameters: Map[String,Array[String]] = Map(), 
		parts: List[Part] = Nil){
}

trait Method{
	def unapply(req:HttpRequest):Option[HttpRequest] = {
		println("req.method: " + req.method + (req.method == this))
		if(req.method == this) Some(req) else None
	}
	def unapply(method:Method):Option[Method] = {
			println("req.method: " + method + (method == this))
			if(method == this) Some(method) else None
	}
}

object Method{
	def apply(method:String):Method = method.toUpperCase match{
		case "GET" => GET
		case "HEAD" => HEAD
		case "POST" => POST
		case "DELETE" => DELETE
		case "TRACE" => TRACE
		case "CONNECT" => CONNECT
		case "OPTIONS" => OPTIONS
		case "PUT" => PUT
		case _ => GET
	}
}

object OPTIONS extends Method
object GET extends Method
object HEAD extends Method
object POST extends Method
object PUT extends Method
object DELETE extends Method
object TRACE extends Method
object CONNECT extends Method


object Path {
	def unapply(req:HttpRequest):Option[List[String]] = Some(req.path) 
}