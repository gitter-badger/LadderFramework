package org.ladderframework
import java.io.InputStream

case class FileInfo(name:String, size:Long, inputStream:InputStream)

object FileInfo{
	
	private val FileName = """.*filename="(.+)"""".r
	
	def apply(part: Part):FileInfo = {
		val contentDisp = part.headers("content-disposition")
		val fileName = contentDisp match {
			case FileName(fn) => fn
			case _ => ""
		}
		FileInfo(fileName, part.size, part.content)
	}
}