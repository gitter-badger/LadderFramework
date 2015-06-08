package org.ladderframework.form

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq
import org.scalatest.FunSpec

import Forms._


class FormBindingSpec extends FunSpec{
	
	val dateMapping = mapping(Date.apply _)( Date.unapply _)(of[Int]("date.day"), of[Int]("date.month"), of[Int]("date.year"))
	
	val speakerMapping = mapping(Speaker.apply _)(Speaker.unapply _)(
		of[String]("name"), 
		dateMapping,
		optional(of[String]("info")),
		list(
				mapping((selected: Boolean, address: Address) => (selected -> address))
						((touple: Tuple2[Boolean, Address]) => Option(touple._1 -> touple._2))(
						boolean, 
						mapping(Address)(Address.unapply _)(of[String]("street"), of[String]("city"), of[Int]("code"))
				)
		)
	)
	
	val form = Form(mapping(Speaker)(Speaker.unapply _)(
		of[String]("name"), 
		mapping(Date.apply _)( Date.unapply _)(of[Int]("date.day"), of[Int]("date.month"), of[Int]("date.year")),
		optional(of[String]("info")),
		list(
				mapping((selected: Boolean, address: Address) => (selected -> address))
						((touple: Tuple2[Boolean, Address]) => Option(touple._1 -> touple._2))(
						boolean("selected"), 
						mapping(Address)(Address.unapply _)(of[String]("street"), of[String]("city"), of[Int]("code"))
				), "addresses"
		)
	))
	
	val fieldMappingDate1: FieldMapping[Int] = dateMapping.mappings._1
	val fieldMappingDate2: FieldMapping[Int] = speakerMapping.mappings._2.mappings._1
	val fieldMappingDate3 = form.mapping.mappings._2.mappings._1
	
	describe("Form bind simple"){
		val per = Speaker("Per", Date(1, 12, 1924), None, Nil)
		it("should handle bind"){
			assert(form.bind(Map("name" -> "Per", "date.day" -> "1", "date.month" -> "12", "date.year" -> "1924", "info" -> "")).get === per)
		}
		it("should handle fill"){
			assert(form.fill(per).value.get === per)
		}
	}
	describe("Form bind advanced"){
		val per = Speaker("Per", Date(1, 12, 1924), Some("jalla"), List(true -> Address("street", "city", 1), false -> Address("highway", "town", 2)))
		it("should handle bind"){
			val bound = form.bind(Map("name" -> "Per", "date.day" -> "1", "date.month" -> "12", "date.year" -> "1924", "info" -> "jalla", 
					"addresses[0].selected" -> "true", "addresses[0].street" -> "street", "addresses[0].city" -> "city", "addresses[0].code" -> "1", 
					"addresses[1].selected" -> "false", "addresses[1].street" -> "highway", "addresses[1].city" -> "town", "addresses[1].code" -> "2" 
					))
			println("bound: " +bound)
			assert(bound.get === per)
		}
		it("should handle fill"){
			assert(form.fill(per).value.get === per)
		}
		it("should find size of list"){
			assert(form.fill(per).context.indexesOf("addresses").size === 2)
		}
	}
	
	describe("Form bind from json"){
  import org.ladderframework.json._
		val per = Speaker("Per", Date(1, 12, 1924), Some("jalla"), List(true -> Address("street", "city", 1), false -> Address("highway", "town", 2)))
		it("should handle bind"){
			val bound = form.bind(JObject("name" -> "Per", 
				"date" -> JObject("day" -> 1, "month" -> 12, "year" -> 1924), 
				"info" -> "jalla",
				"addresses" -> JArray(
					JObject("selected" -> true, "street" -> "street", "city" -> "city", "code" -> 1),
					JObject("selected" -> false, "street" -> "highway", "city" -> "town", "code" -> 2) 
				)
			))
			println("bound: " +bound)
			assert(bound.get === per)
		}
	}
}

case class Speaker(name: String, date: Date, info: Option[String], addresses: List[(Boolean, Address)])
case class Date(day: Int, month: Int, year: Int)
case class Address(street: String, city: String, code: Int)
