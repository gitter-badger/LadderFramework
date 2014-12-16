package org.ladderframework
import java.io.InputStream
import scala.concurrent.Future
import akka.util.ByteString
import akka.stream.scaladsl.Source

case class FileInfo(name:String, inputStream:Source[ByteString])

object FileInfo{
	
	private val FileName = """.*filename="(.+)"""".r
	
	def apply(part: Part):FileInfo = {
		val contentDisp = part.headers("content-disposition")
		val fileName = contentDisp match {
			case FileName(fn) => fn
			case _ => ""
		}
		FileInfo(fileName, part.content)
	}
}