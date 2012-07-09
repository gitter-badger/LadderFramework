package org.ladderframework

import java.net.URL
import java.io.File
import java.io.InputStream
import java.io.FileInputStream

trait DefaultBoot {
	
	def site:PartialFunction[HttpRequest, HttpResponse]
	
	def notFound:HttpResponse = NotFoundResponse
	
	var timeToLivePage: Int = 10 * 60 * 1000
	
	//def resourcePath(resource:String):URL = getClass().getClassLoader().getResource(resource) 
	
	private[ladderframework] var resource:String => URL = s => new File(s).toURI.toURL
	private[ladderframework] var resourceAsStream:String => InputStream = s => new FileInputStream(new File(resource(s).toURI))
	private[ladderframework] var mimeType:String => String = s => s
	private[ladderframework] var contextPath:String = ""
	
}