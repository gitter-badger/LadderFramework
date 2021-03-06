package org.ladderframework

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConverters.mapAsScalaMapConverter
import org.ladderframework.logging.Loggable
import akka.http.model.HttpHeader
import java.io.InputStream
import akka.http.model.headers.HttpCookie
import akka.http.model.Multipart
import akka.stream.scaladsl.Source
import akka.util.ByteString
import scala.concurrent.Future
import akka.stream.FlowMaterializer
import scala.concurrent.ExecutionContext

case class Part(name: String, filename: Option[String], content: Source[ByteString], headers: String => Option[String], size: Long)

object Part{
	def apply(fp: Multipart.FormData.BodyPart): Part = {
		Part(
			name = fp.name,
			filename = fp.filename,
			content = fp.entity.dataBytes,
			headers = fp.headers.map(h => (h.name, h.value)).toMap.get,
			size = 0
		)
	}
}

trait HttpRequest{
	def method:Method
	def headers: String => Option[String] = s => None
	def sessionId:SessionId
	def path:List[String]
	def parameters: Map[String,List[String]]
	//TODO S wrap Part in something appropriate
	def parts: Future[List[Part]] 
	def part(name: String): Future[Option[Part]]
	def partAsString(name: String): Future[Option[String]] 
	def cookies: Seq[Cookie]
}

object HttpRequest{
	def unapply(req: HttpRequest): Option[(Method, List[String])] = {
		import req._
		Option(method, path)
	}
	
}

sealed trait Method{
	def unapply(req:HttpRequest):Option[HttpRequest] = {
		if(req.method == this) Some(req) else None
	}
	def unapply(method:Method):Option[Method] = {
		if(method == this) Some(method) else None
	}
}

case class SessionId(value: String) extends AnyVal

object Method{
	def apply(method:String):Method = method.toUpperCase match{
		case "GET" => GET
		case "HEAD" => HEAD
		case "POST" => POST
		case "DELETE" => DELETE
		case "TRACE" => TRACE
		case "CONNECT" => CONNECT
		case "OPTIONS" => OPTIONS
		case "PUT" => PUT
		case "PATCH" => PUT
		case _ => GET
	}
	case object OPTIONS extends Method
	case object GET extends Method
	case object HEAD extends Method
	case object POST extends Method
	case object PUT extends Method
	case object DELETE extends Method
	case object TRACE extends Method
	case object CONNECT extends Method
	case object PATCH extends Method
}

object &{
	def unapply(req: HttpRequest): Option[(HttpRequest, HttpRequest)] = {
		Option(req, req)
	}
}


object Path {
	def unapply(req:HttpRequest):Option[List[String]] = Some(req.path) 
}
	
object Json{
	def unapply(req: HttpRequest): Option[(List[String], Option[String])] = {
			Option(req.path, req.parameters.keys.headOption)
	}
}

object Session{
	def unapply(req: HttpRequest): Option[SessionId] = {
		Option(req.sessionId)
	}
}

trait Header{
	val name: String
	
	def unapply(req: HttpRequest): Option[String] = {
		req.headers(name)
	}
	
}

object Header{

	/** Content-Types that are acceptable for the response	Accept: text/plain */
	object Accept extends Header{
		val name = "Accept"
	}
	/** Character sets that are acceptable	Accept-Charset: utf-8 */
	object AcceptCharset extends Header{
		val name = "Accept-Charset"
	}
	/** Acceptable encodings. See HTTP compression.	Accept-Encoding: gzip, deflate */
	object AcceptEncoding extends Header{
		val name = "Accept-Encoding"
	}
	/** Acceptable human languages for response	Accept-Language: en-US */
	object AcceptLanguage extends Header{
		val name = "Accept-Language"
	}
	/** Acceptable version in time	Accept-Datetime: Thu, 31 May 2007 20:35:00 GMT */
	object AcceptDatetime extends Header{
		val name = "Accept-Datetime"
	}
	/** Authentication credentials for HTTP authentication	Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ== */
	object Authorization extends Header{
		val name = "Authorization"
	}
	/** Used to specify directives that MUST be obeyed by all caching mechanisms along the request/response chain	Cache-Control: no-cache */
	object CacheControl extends Header{
		val name = "Cache-Control"
	}
	/** What type of connection the user-agent would prefer	Connection: keep-alive */
	object Connection extends Header{
		val name = "Connection"
	}
	/** an HTTP cookie previously sent by the server with Set-Cookie (below)	Cookie: Version=1; Skin=new; */
	object Cookie extends Header{
		val name = "Cookie"
	}
	/** The length of the request body in octets (8-bit bytes)	Content-Length: 348 */
	object ContentLength extends Header{
		val name = "Content-Length"
	}
	/** A Base64-encoded binary MD5 sum of the content of the request body	
	 *  Content-MD5: Q2hlY2sgSW50ZWdyaXR5IQ== */
	object ContentMD5 extends Header{
		val name = "Content-MD5"
	}
	/** The MIME type of the body of the request (used with POST and PUT requests)	
	 *  Content-Type: application/x-www-form-urlencoded */
	object ContentType extends Header{
		val name = "Content-Type"
	}
	/** The date and time that the message was sent	
	 *  Date: Tue, 15 Nov 1994 08:12:31 GMT */
	object Date extends Header{
		val name = "Date"
	}
	/** Indicates that particular server behaviors are required by the client	
	 *  Expect: 100-continue */
	object Expect extends Header{
		val name = "Expect"
	}
	/** 
	 *  The email address of the user making the request	
	 *  From: user@example.com 
	 */
	object From extends Header{
		val name = "From"
	}
	/** The domain name of the server (for virtual hosting), and the TCP port number
	 *   on which the server is listening. The port number may be omitted if the port is the 
	 *   standard port for the service requested.[5] Mandatory since HTTP/1.1. 
	 *   Although domain name are specified as case-insensitive,[6][7] it is not specified 
	 *   whether the contents of the Host field should be interpreted in a case-insensitive manner[8] and 
	 *   in practice some implementations of virtual hosting interpret the contents of the Host field in
	 *    a case-sensitive manner.[citation needed]	
	 *    
	 *    Host: en.wikipedia.org:80 
	 *    Host: en.wikipedia.org
	 */
	object Host extends Header{
		val name = "Host"
	}
	
	/** Only perform the action if the client supplied entity matches the same entity on the server. This is mainly for methods like PUT to only update a resource if it has not been modified since the user last updated it.	If-Match: "737060cd8c284d8af7ad3082f209582d" */
	object IfMatch extends Header{
		val name = "If-Match"
	}
	/** Allows a 304 Not Modified to be returned if content is unchanged	If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT */
	object IfModifiedSince extends Header{
		val name = "If-Modified-Since"
	}
	/** Allows a 304 Not Modified to be returned if content is unchanged, see HTTP ETag	If-None-Match: "737060cd8c284d8af7ad3082f209582d" */
	object IfNoneMatch extends Header{
		val name = "If-None-Match"
	}
	/** If the entity is unchanged, send me the part(s) that I am missing; otherwise, send me the entire new entity	If-Range: "737060cd8c284d8af7ad3082f209582d" */
	object IfRange extends Header{
		val name = "If-Range"
	}
	/** Only send the response if the entity has not been modified since a specific time.	If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT */
	object IfUnmodifiedSince extends Header{
		val name = "If-Unmodified-Since"
	}
	object Location extends Header{
		val name = "Location"
	}
	
	/** Limit the number of times the message can be forwarded through proxies or gateways.	Max-Forwards: 10 */
	object MaxForwards extends Header{
		val name = "Max-Forwards"
	}
	/** Initiates a request for cross-origin resource sharing (asks server for an 'Access-Control-Allow-Origin' response header) .	Origin: http://www.example-social-network.com */
	object Origin extends Header{
		val name = "Origin"
	}
	/** Implementation-specific headers that may have various effects anywhere along the request-response chain.	Pragma: no-cache */
	object Pragma extends Header{
		val name = "Pragma"
	}
	/** Authorization credentials for connecting to a proxy.	Proxy-Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ== */
	object ProxyAuthorization extends Header{
		val name = "Proxy-Authorization"
	}
	/** Request only part of an entity. Bytes are numbered from 0.	Range: bytes=500-999 */
	object Range extends Header{
		val name = "Range"
	}
	/** This is the address of the previous web page from which a link to the currently requested page was followed. (The word “referrer” is misspelled in the RFC as well as in most implementations.)	Referer: http://en.wikipedia.org/wiki/Main_Page */
	object Referer extends Header{
		val name = "Referer"
	}
	/** The transfer encodings the user agent is willing to accept: the same values as for the response header Transfer-Encoding can be used, plus the "trailers" value (related to the "chunked" transfer method) to notify the server it expects to receive additional headers (the trailers) after the last, zero-sized, chunk.	TE: trailers, deflate */
	object TE extends Header{
		val name = "TE"
	}
	/** Ask the server to upgrade to another protocol.	Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11 */
	object Upgrade extends Header{
		val name = "Upgrade"
	}
	/** The user agent string of the user agent	User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20100101 Firefox/12.0 */
	object UserAgent extends Header{
		val name = "User-Agent"
	}
	/** Informs the server of proxies through which the request was sent.	Via: 1.0 fred, 1.1 example.com (Apache/1.1) */
	object Via extends Header{
		val name = "Via"
	}
	/** A general warning about possible problems with the entity body.	Warning: 199 Miscellaneous warning */
	object Warning extends Header{
		val name = "Warning"
	}
}

case class Cookie(name: String, value: String, domain: Option[String] = None, path:Option[String] = None, secure:Boolean = false, maxAge: Option[Long] = None, httpOnly:Boolean = false)

object Cookie{
	def apply(c: HttpCookie):Cookie = {
		Cookie(c.name, c.content , c.domain, c.path, c.secure, c.maxAge, c.httpOnly)
	}
}
