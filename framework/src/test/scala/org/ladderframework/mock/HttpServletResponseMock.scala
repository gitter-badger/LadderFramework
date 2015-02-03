package org.ladderframework.mock

import javax.servlet.http.HttpServletResponse
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.Writer
import java.io.StringWriter
import javax.servlet.ServletOutputStream
import javax.servlet.WriteListener
import javax.servlet.http.{Cookie => jCookie}
import java.util.concurrent.CountDownLatch
import org.ladderframework.SessionId
import scala.collection.mutable.ArrayBuffer

class HttpServletResponseMock() extends HttpServletResponse {

	val stringWriter = new StringWriter
	lazy val printWriter:PrintWriter = new PrintWriter(stringWriter)
	def text = stringWriter.toString()
	
	var status = 0
	var contentType = ""
	var headers:Map[String, String] = Map()
	var cookies = ArrayBuffer[jCookie]()
		
	def getHeaderNames():java.util.Collection[String] = new java.util.ArrayList
	def getHeaders(str: String):java.util.Collection[String] = new java.util.ArrayList
	def getHeader(str: String):String = {""}
	def getStatus():Int = status
	def setStatus(int: Int, str2: String): Unit = {}
	def setStatus(status: Int): Unit = {this.status = status}
	def addIntHeader(str: String, int: Int): Unit = {}
	def setIntHeader(str: String, int: Int): Unit = {}
	def addHeader(key: String, value: String): Unit = {
		headers += (key -> value)
	}
	def setHeader(key: String, value: String): Unit = {
		headers += (key -> value)
	}
	def addDateHeader(str: String, x$2: Long): Unit = {}
	def setDateHeader(str: String, x$2: Long): Unit = {}
	def sendRedirect(str: String): Unit = {}
	def sendError(int: Int): Unit = {}
	def sendError(int: Int, str2: String): Unit = {}
	def encodeRedirectUrl(str: String):String = str
	def encodeUrl(str: String):String = str
	def encodeRedirectURL(str: String):String = str
	def encodeURL(str: String):String = str
	def containsHeader(str: String):Boolean = false
	def addCookie(cookie: jCookie): Unit = {
		cookies += (cookie)
		println("Adding cookie: " + cookie.getName + " -> " + cookie.getValue)
	}
	def getLocale():java.util.Locale = java.util.Locale.getDefault()
	def setLocale(locale: java.util.Locale): Unit = {}
	def reset(): Unit = {}
	def isCommitted():Boolean = false
	def resetBuffer(): Unit = {}
	def flushBuffer(): Unit = {}
	def getBufferSize():Int = 8192
	def setBufferSize(int: Int): Unit = {}
	def setContentType(contentType: String): Unit = {
		this.contentType = contentType
	}
	def setContentLength(int: Int): Unit = {}
	def setContentLengthLong(long: Long): Unit = {}
	def setCharacterEncoding(str: String): Unit = {}
	def getWriter():PrintWriter = printWriter
	def getOutputStream():ServletOutputStream = new ServletOutputStream{
		override def isReady() = true
		override def write(int: Int): Unit = {
			stringWriter.write(int)
		}
		override def setWriteListener(writeListener: WriteListener ): Unit = ???
	}
	def getContentType():String = ""
	def getCharacterEncoding():String = ""

	
	
}