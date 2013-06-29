package org.ladderframework.js

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.FeatureSpec
import org.ladderframework.js._
import org.json4s.JsonDSL._

@RunWith(classOf[JUnitRunner])
class JsCmdSpec extends FunSpec with GivenWhenThen {

	it("should escape input") {
		assert(JsCall("alert", "hei").toCmd === "alert(\"hei\");")
	}
	
}

class JsInterpolationSpec extends FeatureSpec{
 
  feature("JavaScript Interpolation"){
		scenario("simple command") {
			assert( (js"alert('simple')").toCmd === "alert('simple')")
		}
 
		scenario("with JString") {
			val string = "some string"
			assert( (js"alert(${string})").toCmd === """alert("some string")""")
		}
 
		scenario("with JInt") {
			val int = 1
			assert( (js"alert(${int})").toCmd === """alert(1)""")
		}
 
		scenario("with JObject") {
			pending
			//case class Person(name: String, age: Int)
			//val person = Person(name = "Name", age = 30)
			//assert( js"alert(${person})".toJsCmd === JsRaw("""alert({name: "Name", age: 30});""").toJsCmd)
		}
	}
 
}