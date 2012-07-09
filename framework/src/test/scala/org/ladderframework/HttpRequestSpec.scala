package org.ladderframework

import org.scalatest.FunSuite
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen

class HttpRequestSpec extends FunSpec with GivenWhenThen {

  describe("Method") {
  	it("should map GET") {
  		assert(Method("GET") == GET)
  	}
  	it("should map POST") {
  		assert(Method("POST") == POST)
  	}
  	it("should map *") {
  		pending
  	}
  }
  
}