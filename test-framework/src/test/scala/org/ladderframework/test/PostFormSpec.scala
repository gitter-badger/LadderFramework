package org.ladderframework.test

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import bootstrap.LadderBoot
import org.ladderframework.Context
import org.ladderframework.test.response._
import org.ladderframework.test.page._
import org.ladderframework.js._
import org.ladderframework.HttpResponse
import org.ladderframework.Utils

class PostFormSpec extends FunSpec with GivenWhenThen {
	
	LadderBoot.resource = (resource:String) => { 
		getClass().getClassLoader().getResource(resource) 
	}  
	
	implicit val context: Context = Context(
		contextID = Utils.uuid, 
		addResponse = (path: List[String], res: HttpResponse) => "", 
		update = (str: JsCmd) => Unit )
		
	val indexResponse = new IndexResponse
	
	val indexPageObject = IndexPageObject(indexResponse)
	
	describe("PageContent") {
  	it("should contain text") {
  		//val aboutPageObject = indexPageObject.aboutLink.click[AboutPageObject]
  		//assert(aboutPageObject.header.text === "about header")
  	}
	}

}