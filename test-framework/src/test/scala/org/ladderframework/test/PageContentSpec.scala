package org.ladderframework.test

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.ladderframework.Context
import org.ladderframework.Utils
import org.ladderframework.HttpResponse
import org.ladderframework.test.page._
import org.ladderframework.test.response._
import bootstrap.LadderBoot

class PageContentSpec extends FunSpec with GivenWhenThen {
	
	LadderBoot.resource = (resource:String) => { 
		getClass().getClassLoader().getResource(resource) 
	}  
	
	implicit val context: Context = Context(
		contextID = Utils.uuid, 
		addResponse = (path: List[String], res: HttpResponse) => "", 
		update = (str: String) => Unit )
		
	val indexResponse = new IndexResponse
	
	val indexPageObject = IndexPageObject(indexResponse)
	
	describe("PageContent") {
  	it("should contain text") {
  		assert(indexPageObject.header.text === "index header")
  	}
	}

}