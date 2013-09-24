package org.ladderframework

import org.scalatest.FunSuite
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import javax.servlet.http.HttpServletResponse
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class HttpRequestSpec extends FunSpec with GivenWhenThen {

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
  	def applyToHttpServletResponse(httpResponseOutput: HttpResponseOutput)(implicit context: Context, ec: ExecutionContext): Future[Status] = Future.successful(status) 
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
  		assert(HttpReq(GET, "sessionID", "path" :: Nil) match {
  			case Session(session) => session == "sessionID"
  		})
  	}
  	it("should match on path and extract on Session id"){
  		assert(HttpReq(GET, "sessionID", "path" :: Nil) match {
  			case Path("path" :: Nil) & Session(session) => session == "sessionID"
  		})
  	}
  	it("should match on path and json content"){
  		assert(HttpReq(GET, Map("Accept" -> Option("application/json")), "sessionID", "path" :: Nil) match {
  			case Path("path" :: Nil) & Session(session) & Accept("application/json") => session == "sessionID"
  		})
  	}
  	
  }
  
  object HttpReq{
  	def apply(givenMethod: Method, givenPath: List[String]) = new HttpRequest{
  		override val method = givenMethod
  		override def sessionID = ""
  		override def path = givenPath
  		override def parameters = Map()
  	}
  	def apply(givenMethod: Method, givenSession: String, givenPath: List[String]) = new HttpRequest{
  		override val method = givenMethod
  		override def sessionID = givenSession
  		override def path = givenPath
  		override def parameters = Map()
  	}
  	def apply(givenMethod: Method, givenHeaders: Map[String, Option[String]], givenSession: String, givenPath: List[String]) = new HttpRequest{
  		override val method = givenMethod
  		override def sessionID = givenSession
			override def path = givenPath
			override def headers = givenHeaders
			override def parameters = Map()
  	}
  }
  
}