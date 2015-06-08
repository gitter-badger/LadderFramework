package org.ladderframework.test

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.ladderframework.Context
import org.ladderframework.test.response._
import org.ladderframework.test.page._
import org.ladderframework.js._
import org.ladderframework.HttpResponse
import org.ladderframework.utils
import scala.util.Try
import org.ladderframework.DefaultBoot

class PostFormSpec extends FunSpec with GivenWhenThen {
	
	val boot = new DefaultBoot{
		def site = {
			case _ => ???
		}
	}    
	
	implicit val context: Context = Context(
		contextID = utils.uuid, 
		addResponse = (path: List[String], res: HttpResponse) => "", 
		update = (str: JsCmd) => Try{}, boot)
		
	val indexResponse = new IndexResponse
	
	val indexPageObject = IndexPageObject(indexResponse)
	
	describe("PageContent") {
  	it("should contain text") {
  		//val aboutPageObject = indexPageObject.aboutLink.click[AboutPageObject]
  		//assert(aboutPageObject.header.text === "about header")
  	}
	}

}