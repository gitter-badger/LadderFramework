package org.ladderframework

import java.io.OutputStream
import java.io.Writer
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.{Cookie => SCookie}
import java.nio.charset.Charset
import java.io.InputStream

case class MediaType(value: String)
case class ContentType(mediaType: MediaType, charset: Option[Charset])

object ContentType{
	val `text/html` = ContentType(MediaType("text/html"), Some(Charset.forName("UTF-8")))
	val `text/plain` = ContentType(MediaType("text/plain"), Some(Charset.forName("UTF-8")))
	val `application/json` = ContentType(MediaType("application/json"), Some(Charset.forName("UTF-8")))
	val `application/xml` = ContentType(MediaType("application/xml"), Some(Charset.forName("UTF-8")))
	val `text/javascript` = ContentType(MediaType("text/javascript"), Some(Charset.forName("UTF-8")))
}

trait HttpResponseOutput {
	self =>
	type C
	def status: Status
	def contentType: ContentType
	def headers: Seq[HeaderValue]
	def cookies: Seq[Cookie]
	def :+ (c: Cookie) = self 
	def :+ (h: HeaderValue) = self 
	def content: C
}

case class HttpStringResponseOutput(
	status: Status,
	contentType: ContentType,
	headers: Seq[HeaderValue] = Nil,
	cookies: Seq[Cookie] = Nil,
	content: String
) extends HttpResponseOutput{
	type C = String
	override def :+ (c: Cookie) = copy(cookies = cookies :+ c)
	override def :+ (h: HeaderValue) = copy(headers = headers :+ h)
}

case class HttpStreamResponseOutput(
	status: Status,
	contentType: ContentType,
	headers: Seq[HeaderValue] = Nil,
	cookies: Seq[Cookie] = Nil,
	content: InputStream
) extends HttpResponseOutput{
	type C = InputStream
	override def :+(c: Cookie) = copy(cookies = cookies :+ c)
	override def :+ (h: HeaderValue) = copy(headers = headers :+ h)
}

case class HttpPathResponseOutput(
	status: Status,
	contentType: ContentType,
	headers: Seq[HeaderValue] = Nil,
	cookies: Seq[Cookie] = Nil,
	content: java.nio.file.Path
) extends HttpResponseOutput{
	type C = java.nio.file.Path
	override def :+(c: Cookie) = copy(cookies = cookies :+ c)
	override def :+ (h: HeaderValue) = copy(headers = headers :+ h)
}
