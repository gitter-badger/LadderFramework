package org.ladderframework.json

import org.scalacheck._
import Arbitrary._
import Prop._
import Gen._
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers
import org.scalatest.prop.PropertyChecks

class JsonSpec extends FunSpec with Checkers {

	implicit lazy val jValue: Arbitrary[JValue] = Arbitrary {
		val maxDepth = 100
		def jString: Gen[JString] = for { st <- arbitrary[String] if st.matches("""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""")} yield JString(st)
		def jInt: Gen[JInt] = for { i <- arbitrary[Int]} yield JInt(i)
		def jDouble: Gen[JDouble] = for { i <- arbitrary[Double]} yield JDouble(i)
		def jBoolean: Gen[JBoolean] = for { i <- arbitrary[Boolean]} yield JBoolean(i)
		def jNull = value(JNull)
		def jObject(depth: Int): Gen[JObject] = {
			for{ 
				obj <- sized(s => containerOfN[Array, (String, JValue)](math.min(s, maxDepth), for{
					key <- arbitrary[String] if key.matches("""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""")
					value <- jVal(depth)
				} yield key -> value)) if depth < maxDepth
			} yield JObject(obj:_*)
		}
		def jArray(depth: Int): Gen[JArray] = for{
				ar <- sized(s => containerOfN[Array, JValue](math.min(s, maxDepth), jVal(depth + 1)))  if depth < maxDepth
			} yield JArray(ar: _*)
	
		def jVal(depth: Int) = oneOf(jString, jInt, jDouble, jBoolean, jArray(depth + 1), jNull, jObject(depth + 1))
		
		jVal(0)
	}
	
	it("parse nospace") {
		check(forAll{(value: JValue) => value.nospace.toJson.get == value})
	}
	
	it("parse pretty"){
		check(forAll{(value: JValue) => value.pretty.toJson.get == value})
	}
	
	
	
}