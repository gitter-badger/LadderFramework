package org.ladderframework.html.form

import org.ladderframework.HtmlPage
import org.scalatest.FunSpec
import org.ladderframework.Context
import org.ladderframework.html.form.Formats._
import org.ladderframework.html.form.Forms._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq
import Forms._
import Formats._
import org.ladderframework.html.form._
import org.ladderframework.js.JsCmd
import org.ladderframework.js.JsNoCmd
import org.ladderframework.css.CssSelector._

class SpeakerPageSpec extends FunSpec{
	
	val dateMapping = mapping(Date.apply _)( Date.unapply _)(of[Int]("date.day"), of[Int]("date.month"), of[Int]("date.year"))
	
	val speakerMapping = mapping(Speaker.apply _)(Speaker.unapply _)(
		of[String]("name"), 
		dateMapping,
		of[String]("info")
	)
	
	val form = Form(mapping(Speaker)(Speaker.unapply _)(
		of[String]("name"), 
		mapping(Date.apply _)( Date.unapply _)(of[Int]("date.day"), of[Int]("date.month"), of[Int]("date.year")),
		of[String]("info")
	))
	
	val fieldMappingDate1: FieldMapping[Int] = dateMapping.mappings._1
	val fieldMappingDate2: FieldMapping[Int] = speakerMapping.mappings._2.mappings._1
	val fieldMappingDate3 = form.mapping.mappings._2.mappings._1
	
	describe("Form"){
		val per = Speaker("Per", Date(1, 12, 1924), "")
		it("should handle bind"){
			assert(form.bind(Map("name" -> "Per", "date.day" -> "1", "date.month" -> "12", "date.year" -> "1924", "info" -> "")).get === per)
		}
		it("should handle fill"){
			assert(form.fill(per).value.get === per)
		}
	}
}

case class Speaker(name: String, date: Date, info: String)
case class Date(day: Int, month: Int, year: Int)


class ProductHtmlPage(speaker: Option[Speaker]) extends HtmlPage {
	val source: String = "speaker.html"
		
	val form = Form(mapping(Speaker)(Speaker.unapply _)(
		of[String], 
		mapping(Date.apply _)( Date.unapply _)(of[Int], of[Int], of[Int]),
		of[String]
	))
	
	def render(implicit context: Context, ec: ExecutionContext): Future[NodeSeq => NodeSeq] = Future{
		Ajax(form)((either, id) => JsNoCmd)(
				implicit context => speaker => {
					ns => {
						<div>
							<h3>Speaker</h3>
							{
								speaker.render((name, date, info) => {
									date.render((day, month, year) => ns => {
										name.text("Name", 'id -> "name") ++
										<fieldset>
											<legend>Date of birth:</legend>
											{
												day.text("Day") ++
												day.text("Month") ++
												day.text("Year")
											}
										</fieldset> ++
										info.textarea("Info")
									})
								})
							}
							<input type="submit" value="Send"/>
						</div>
					}
				}
		)
		ns => ns
	}
}