package org.ladderframework

import org.scalatest.FunSuite
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen

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
  
}