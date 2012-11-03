package org.ladderframework.js

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen

@RunWith(classOf[JUnitRunner])
class JsCmdSpec extends FunSpec with GivenWhenThen {

	it("should escape input") {
		assert(JsCall("alert", "hei").toCmd === "alert(\"hei\");")
	}
	
}