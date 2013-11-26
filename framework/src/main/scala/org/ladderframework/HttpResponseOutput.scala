package org.ladderframework

import java.io.OutputStream
import java.io.Writer
import javax.servlet.http.HttpServletResponse

trait HttpResponseOutput {
	def setStatus(status: Status): Unit
	def setContentType(contentType: String): Unit
	def setHeader(key: Header, value: String): Unit
	def outputStream: OutputStream
	def writer: Writer
}

class HttpServletResponseOutput(hsr: HttpServletResponse) extends HttpResponseOutput{
	
	def setStatus(status: Status){
		hsr.setStatus(status.code)
	}
	
	def setContentType(contentType: String){
		hsr.setContentType(contentType)
	}
	
	def setHeader(key: Header, value: String){
		hsr.setHeader(key.name, value)
	}
	
	def outputStream: OutputStream = hsr.getOutputStream()
	
	def writer: Writer = hsr.getWriter()
}