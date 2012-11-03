package org.ladderframework.test

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.ladderframework.Context
import org.ladderframework.Utils
import org.ladderframework.HttpResponse
import org.ladderframework.js._
import org.ladderframework.test.page._
import org.ladderframework.test.response._
import bootstrap.LadderBoot
import org.scalatest.concurrent.Futures
import scala.util.Success

class PageContentSpec extends FunSpec with GivenWhenThen with Futures{

	LadderBoot.resourceImpl = (resource:String) => { 
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
  		val Success(header) = indexPageObject.header.future.value.get
  		assert(header.text === "index header")
  	}
	}

}