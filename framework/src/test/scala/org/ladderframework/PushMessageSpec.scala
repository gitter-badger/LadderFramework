package org.ladderframework

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.ladderframework.js._
import org.scalatest.junit.JUnitRunner

class PushMessageSpec extends FunSpec with GivenWhenThen {
	
	it("should escape input") {
		assert(PushMessage(id = "testID", message = JsCall("alert", "hei").toCmd).asJson === 
			"{\"id\":\"testID\", \"message\":\"alert(\\\"hei\\\");\"}")
	}

}