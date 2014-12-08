package org.ladderframework

import java.io.OutputStream
import java.io.InputStream
import akka.http.model.HttpHeader
import java.nio.charset.Charset

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
	type C
	def status: Status
	def contentType: ContentType
	def headers: Seq[HttpHeader]
	def cookies: Seq[Cookie]
	def content: C
}

case class HttpStringResponseOutput(
	status: Status,
	contentType: ContentType,
	headers: Seq[HttpHeader] = Nil,
	cookies: Seq[Cookie] = Nil,
	content: String
) extends HttpResponseOutput{
	type C = String
}

case class HttpStreamResponseOutput(
	status: Status,
	contentType: ContentType,
	headers: Seq[HttpHeader] = Nil,
	cookies: Seq[Cookie] = Nil,
	content: InputStream
) extends HttpResponseOutput{
	type C = InputStream
}