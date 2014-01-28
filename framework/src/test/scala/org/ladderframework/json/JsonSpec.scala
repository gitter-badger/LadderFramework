package org.ladderframework.json

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen

class JsonSpec extends FunSpec {

	it("parser the rendered") {
		val value = JObject("string" -> "String", "boolean" -> true, "array" -> JArray("stein", false, true, JNull, 1 , 1.2))
		assert(value.nospace.toJson.get === value)
	}
	
	it("shoutld render pretty") {
		val value = JObject("string" -> "String", "boolean" -> true, "array" -> JArray("stein", false, true, JNull, 1 , 1.2), "nested" -> JObject("string" -> "String", "boolean" -> true, "array" -> JArray("stein", false, true, JNull, 1 , 1.2)))
		assert(value.pretty.toJson.get === value)
	}
	
}