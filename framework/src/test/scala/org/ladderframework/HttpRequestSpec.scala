package org.ladderframework

import org.scalatest.FunSuite
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import Method._
import org.ladderframework.Header.Accept

class HttpRequestSpec extends FunSpec with GivenWhenThen {
	import Method._
	
  describe("Method") {
  	it("should map GET") {
  		assert(Method("GET") === GET)
  	}
  	it("should map POST") {
  		assert(Method("POST") === POST)
  	}
  	it("should map PUT") {
  		assert(Method("PUT") === PUT)
  	}
  	it("should map HEAD") {
  		assert(Method("HEAD") === HEAD)
  	}
  	it("should map DELETE") {
  		assert(Method("DELETE") === DELETE)
  	}
  	it("should map TRACE") {
  		assert(Method("TRACE") === TRACE)
  	}
  	it("should map OPTIONS") {
  		assert(Method("OPTIONS") === OPTIONS)
  	}
  	it("should map CONNECT") {
  		assert(Method("CONNECT") === CONNECT)
  	}
  }
  
  val dummyResponse = new HttpResponse{
  	def status:Status = OK 
  	def httpResponse()(implicit context: Context, ec: ExecutionContext): Future[HttpStringResponseOutput] = 
  		Future.successful(HttpStringResponseOutput(status = status, contentType = ContentType.`text/plain` , content = "")) 
  }
  
  describe("Matching HttpRequest") {
  	
  	it("should match on Path"){
  		assert(HttpReq(GET, "path" :: Nil) match {
  			case Path("path" :: Nil) => true
  		})
  	}
  	it("should match on Method"){
  		assert(HttpReq(GET, "path" :: Nil) match {
  			case GET(_) => true
  		})
  	}
  	it("should extract on Session id"){
  		assert(HttpReq(GET, "path" :: Nil, SessionId("sessionID")) match {
  			case Session(session) => session == SessionId("sessionID")
  		})
  	}
  	it("should match on path and extract on Session id"){
  		assert(HttpReq(GET,"path" :: Nil, SessionId("sessionID")) match {
  			case Path("path" :: Nil) & Session(session) => session == SessionId("sessionID")
  		})
  	}
  	it("should match on path and json content"){
  		assert(HttpReq(GET, "path" :: Nil, SessionId("sessionID"), Map("Accept" -> Option("application/json"))) match {
  			case Path("path" :: Nil) & Session(session) & Accept("application/json") => session == SessionId("sessionID")
  		})
  	}
  	
  }
  
  object HttpReq{
  	def apply(givenMethod: Method, givenPath: List[String], givenSession: SessionId = SessionId(""), givenHeaders: Map[String, Option[String]] = Map()) = new HttpRequest{
  		override val method = givenMethod
  		override def sessionId = givenSession
			override def path = givenPath
			override def headers = givenHeaders
			override def parameters = Map()
			override def cookies = Nil
			override def parts: Future[List[Part]] = Future.successful(Nil) 
			override def part(name: String): Future[Option[Part]] = Future.successful(None)
			override def partAsString(name: String): Future[Option[String]] = Future.successful(None) 
  	}
  }
  
}