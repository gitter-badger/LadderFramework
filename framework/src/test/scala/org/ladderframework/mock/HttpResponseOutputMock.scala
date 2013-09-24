package org.ladderframework.mock

import java.io.PrintWriter
import java.io.StringWriter
import org.ladderframework.HttpResponseOutput
import org.ladderframework.NotImplemented
import org.ladderframework.Status
import javax.servlet.ServletOutputStream
import java.io.OutputStream

class HttpResponseOutputMock extends HttpResponseOutput {

	val stringWriter = new StringWriter
	lazy val printWriter:PrintWriter = new PrintWriter(stringWriter)
	def text = stringWriter.toString()
	
	var status: Status = NotImplemented
	var contentType = ""
	var headers:Map[String, String] = Map()
		
	def setStatus(status: Status) {this.status = status}
	def setHeader(key: String, value: String) {
		headers += (key -> value)
	}
	def setContentType(contentType: String) {
		this.contentType = contentType
	}
	def writer:PrintWriter = printWriter
	def outputStream:OutputStream = new ServletOutputStream{
		override def write(int: Int): Unit = {
			stringWriter.write(int)
		}
	}
}