package org.ladderframework.test

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import bootstrap.LadderBoot
import org.ladderframework.Context
import org.ladderframework.test.response._
import org.ladderframework.test.page._
import org.ladderframework.js._
import org.ladderframework.HttpResponse
import org.ladderframework.Utils
import scala.util.Try

class PostFormSpec extends FunSpec with GivenWhenThen {
	
	LadderBoot.resourceImpl = (resource:String) => { 
		getClass().getClassLoader().getResource(resource) 
	}  
	
	implicit val context: Context = Context(
		contextID = Utils.uuid, 
		addResponse = (path: List[String], res: HttpResponse) => "", 
		update = (str: JsCmd) => Try{} )
		
	val indexResponse = new IndexResponse
	
	val indexPageObject = IndexPageObject(indexResponse)
	
	describe("PageContent") {
  	it("should contain text") {
  		//val aboutPageObject = indexPageObject.aboutLink.click[AboutPageObject]
  		//assert(aboutPageObject.header.text === "about header")
  	}
	}

}