package org.ladderframework.validation

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.junit.JUnitRunner
import org.ladderframework.html.form.TextInput
import org.ladderframework.html.form.InputValueElement
import scalaz._
import Scalaz._
import org.ladderframework.html.validation._
import org.ladderframework.html.validation.ExpectConvertValidate._
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class FieldValidationSpec extends FunSpec with GivenWhenThen with ShouldMatchers{
	describe("A Optional field") {
		val optionTextElement = optional[String] 
		val optionIntElement = optional =>> convert(toInt) =>> validate( Min(0) )
		
		it("should handle blank text input") {
			optionTextElement("".some) should be === "".some.success
		}
		
		it("should handle none text input") {
			optionTextElement(None) should be === None.success
		}
		
		it("should handle text for text input") {
			val in = "sk".some
			optionTextElement(in) should be === (in.success)
		}
		
		it("should handle wrong type input") {
			assertFail(optionIntElement("tull".some), "tull is not an int")
		}
		
		it("should handle right type input") {
			optionIntElement("1".some) should be === 1.some.success
		}
		
		it("should handle wrong validation input") {
			assertFail(optionIntElement("-1".some), "-1 is to low")
		}
		
		it("should handle right validation input") {
			optionIntElement("1".some) should be === 1.some.success
		}
	}
	
	describe("A Mandatory field") {
		val mandatoryTextElement = mandatory[String]
		val mandatoryIntElement = mandatory =>> convert(toInt) =>> validate( Min(0) )
		
		it("should handle blank for text input") {
			assertFail(mandatoryTextElement("".some), "missing value")
		}
		
		it("should handle none for text input") {
			assertFail(mandatoryTextElement(None), "missing value")
		}
		
		it("should handle text for text input") {
			mandatoryTextElement("yes".some) should be === "yes".success
		}
		
		it("should handle wrong type input") {
			assertFail(mandatoryIntElement("tull".some), "tull is not an int")
		}
		
		it("should handle right type input") {
			mandatoryIntElement("1".some) should be ===  1.success
		}
		
		it("should handle wrong validation input") {
			assertFail(mandatoryIntElement("-1".some), "-1 is to low")
		}
		
		it("should handle right validation input") {
			mandatoryIntElement("1".some) should be ===  1.success
		}
	}
	
	def assertFail(found: ValidationNEL[ValidationError, _], expectedFail: String) {
		found match {
			case Failure(e) => e.head.msg should be ===  expectedFail
			case _ => fail()
		}
		
	}

}