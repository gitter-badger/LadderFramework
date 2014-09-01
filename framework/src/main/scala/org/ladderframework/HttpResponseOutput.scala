package org.ladderframework

import java.io.OutputStream
import java.io.Writer
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.{Cookie => SCookie} 

trait HttpResponseOutput {
	def setStatus(status: Status):Unit
	def setContentType(contentType: String):Unit
	def setHeader(key: Header, value: String):Unit
	def addCookie(cookie: Cookie):Unit
	def outputStream: OutputStream
	def writer: Writer
}

class HttpServletResponseOutput(hsr: HttpServletResponse) extends HttpResponseOutput{
	
	def setStatus(status: Status): Unit = {
		hsr.setStatus(status.code)
	}
	
	def setContentType(contentType: String): Unit = {
		hsr.setContentType(contentType)
	}
	
	def setHeader(key: Header, value: String): Unit = {
		hsr.setHeader(key.name, value)
	}
	
	def outputStream: OutputStream = hsr.getOutputStream()
	
	def writer: Writer = hsr.getWriter()
	
	def addCookie(cookie: Cookie): Unit = {
		import cookie._
		val sCookie = new SCookie(name, value)
		comment.foreach(sCookie.setComment)
		domain.foreach(sCookie.setDomain)
		sCookie.setHttpOnly(httpOnly)
		sCookie.setMaxAge(maxAge)
		path.foreach(sCookie.setPath)
		sCookie.setSecure(secure)
//		sCookie.setVersion(version)
		hsr.addCookie(sCookie)
	}
}