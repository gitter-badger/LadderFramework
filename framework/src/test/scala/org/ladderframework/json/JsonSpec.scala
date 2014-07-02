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
		val jString: Gen[JString] = for { st <- arbitrary[String] if st.matches("""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""")} yield JString(st)
		val jInt: Gen[JInt] = for { i <- arbitrary[Int]} yield JInt(i)
		val jDouble: Gen[JDouble] = for { i <- arbitrary[Double]} yield JDouble(i)
		val jBoolean: Gen[JBoolean] = for { i <- arbitrary[Boolean]} yield JBoolean(i)
		def jNull = Gen.const(JNull)
		
		lazy val arrayParams = Gen.resize(5, containerOf[Array, JValue](jVal))
		
		lazy val jArray: Gen[JArray] = for{
				ar <- arrayParams
		} yield JArray(ar: _*)
		
		lazy val objParams = Gen.resize(5, containerOf[Array, (String, JValue)]( 
				for{
					key <- jString 
					value <- jVal
				} yield key.value -> value
		))
		
		lazy val jObject: Gen[JObject] = {
			for{ 
				obj <- objParams
			} yield JObject(obj:_*)
		}

		lazy val jVal = Gen.lzy(oneOf(jString, jInt, jDouble, jBoolean, jArray, jNull, jObject))
		
		jVal
	}
	
	it("should parse nospace") {
		check(forAll{(value: JValue) => value.nospace.toJson.get == value})
	}
	
	it("should parse pretty"){
		check(forAll{(value: JValue) => value.pretty.toJson.get == value})
	}
	
	
	
}