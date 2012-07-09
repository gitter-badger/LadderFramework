package org.ladderframework.mock

import javax.servlet.http.HttpServletResponse
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.Writer
import java.io.StringWriter
import javax.servlet.ServletOutputStream

class HttpServletResponseMock extends HttpServletResponse {

	val stringWriter = new StringWriter
	lazy val printWriter:PrintWriter = new PrintWriter(stringWriter)
	def text = stringWriter.toString()
	
	var status = 0
	var contentType = ""
	var headers:Map[String, String] = Map()
		
	def getHeaderNames():java.util.Collection[String] = new java.util.ArrayList
	def getHeaders(str: String):java.util.Collection[String] = new java.util.ArrayList
	def getHeader(str: String):String = {""}
	def getStatus():Int = status
	def setStatus(int: Int, str2: String) {}
	def setStatus(status: Int) {this.status = status}
	def addIntHeader(str: String, int: Int) {}
	def setIntHeader(str: String, int: Int) {}
	def addHeader(str: String, str2: String) {}
	def setHeader(key: String, value: String) {
		headers += (key -> value)
	}
	def addDateHeader(str: String, x$2: Long) {}
	def setDateHeader(str: String, x$2: Long) {}
	def sendRedirect(str: String) {}
	def sendError(int: Int) {}
	def sendError(int: Int, str2: String) {}
	def encodeRedirectUrl(str: String):String = str
	def encodeUrl(str: String):String = str
	def encodeRedirectURL(str: String):String = str
	def encodeURL(str: String):String = str
	def containsHeader(str: String):Boolean = false
	def addCookie(cookie: javax.servlet.http.Cookie) {}
	def getLocale():java.util.Locale = java.util.Locale.getDefault()
	def setLocale(locale: java.util.Locale) {}
	def reset() {}
	def isCommitted():Boolean = false
	def resetBuffer() {}
	def flushBuffer() {}
	def getBufferSize():Int = 8192
	def setBufferSize(int: Int) {}
	def setContentType(contentType: String) {
		this.contentType = contentType
	}
	def setContentLength(int: Int) {}
	def setCharacterEncoding(str: String) {}
	def getWriter():PrintWriter = printWriter
	def getOutputStream():ServletOutputStream = new ServletOutputStream{
		override def write(int: Int): Unit = {
			stringWriter.write(int)
		}
	}
	def getContentType():String = ""
	def getCharacterEncoding():String = ""

	
	
}