package org.ladderframework.form

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq
import org.scalatest.FunSpec

import named._


class FormBindingSpec extends FunSpec{
	
	val dateMapping = "date" --> mapping(Date.apply _)( Date.unapply _)("day" --> of[Int], "month" --> of[Int], "year" --> of[Int])
  val addressMapping = "address" --> mapping(Address)(Address.unapply _)("street" --> of[String], "city" --> of[String], "code" --> of[Int])
	
	val speakerMapping = "speaker" --> mapping(Speaker.apply _)(Speaker.unapply _)(
		"name" --> of[String], 
		dateMapping,
		"info" --> optional(of[String]),
		"addresses" --> list(
				mapping((selected: Boolean, address: Address) => (selected -> address))
						((touple: Tuple2[Boolean, Address]) => Option(touple._1 -> touple._2))(
						"selected" --> boolean, 
						addressMapping
				)
		)
	)
	
	val form = Form(speakerMapping)
	
	val fieldMappingDate1: FieldMapping[Int] = dateMapping.mappings._1
	val fieldMappingDate2: FieldMapping[Int] = speakerMapping.mappings._2.mappings._1
	val fieldMappingDate3 = form.mapping.mappings._2.mappings._1
	
  describe("mapping binding simple"){
    it("should handle field"){
      val nameMapping = "name" -> of[String]
      assert(nameMapping.bind(Map("name" -> "Per"), "") === Right("Per"))
    }
    it("should handle compunded mapping"){
      assert(dateMapping.bind(Map("date.day" -> "1", "date.month" -> "2", "date.year" -> "1980"), "") === Right(Date(1, 2, 1980)))
    }
    it("should handle optional mapping"){
      val optInfo = "info" -> optional(of[String])
      assert(optInfo.bind(Map("info" -> "super duper info"), "") === Right(Some("super duper info")))
    }
    
    it("should handle list mapping"){
    	assert(addressMapping.bind(Map("address.street" -> "small street", "address.city" -> "Big city", "address.code" -> "12345"), "") === Right(Address("small street","Big city",12345)))
    }
  }
  
	describe("Form bind simple"){
		val per = Speaker("Per", Date(1, 12, 1924), None, Nil)
		it("should handle bind"){
			assert(form.bind(Map("speaker.name" -> "Per", "speaker.date.day" -> "1", "speaker.date.month" -> "12", "speaker.date.year" -> "1924", "speaker.info" -> "")).get === per)
		}
		it("should handle fill"){
			assert(form.fill(per).value.get === per)
		}
	}
	describe("Form bind advanced"){
		val per = Speaker("Per", Date(1, 12, 1924), Some("jalla"), List(true -> Address("street", "city", 1), false -> Address("highway", "town", 2)))
		it("should handle bind"){
			val bound = form.bind(Map("speaker.name" -> "Per", "speaker.date.day" -> "1", "speaker.date.month" -> "12", "speaker.date.year" -> "1924", "speaker.info" -> "jalla", 
					"speaker.addresses[0].selected" -> "true", "speaker.addresses[0].address.street" -> "street", "speaker.addresses[0].address.city" -> "city", "speaker.addresses[0].address.code" -> "1", 
					"speaker.addresses[1].selected" -> "false", "speaker.addresses[1].address.street" -> "highway", "speaker.addresses[1].address.city" -> "town", "speaker.addresses[1].address.code" -> "2" 
					))
			println("bound: " +bound)
			assert(bound.get === per)
		}
		it("should handle fill"){
			assert(form.fill(per).value.get === per)
		}
		it("should find size of list"){
			assert(form.fill(per).context.indexesOf("speaker.addresses").size === 2)
		}
	}
	
	describe("Form bind from json"){
  import org.ladderframework.json._
		val per = Speaker("Per", Date(1, 12, 1924), Some("jalla"), List(true -> Address("street", "city", 1), false -> Address("highway", "town", 2)))
		it("should handle bind"){
			val bound = form.bind(JObject("speaker" -> JObject("name" -> "Per", 
				"date" -> JObject("day" -> 1, "month" -> 12, "year" -> 1924), 
				"info" -> "jalla",
				"addresses" -> JArray(
					JObject("selected" -> true, "address" -> JObject("street" -> "street", "city" -> "city", "code" -> 1)),
					JObject("selected" -> false, "address" -> JObject("street" -> "highway", "city" -> "town", "code" -> 2)) 
				)
			)))
			println("bound: " +bound)
			assert(bound.get === per)
		}
	}
}

case class Speaker(name: String, date: Date, info: Option[String], addresses: List[(Boolean, Address)])
case class Date(day: Int, month: Int, year: Int)
case class Address(street: String, city: String, code: Int)
