package org.ladderframework.css

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import CssSelector._

class CssSelectorSpec extends FunSpec with GivenWhenThen {

  describe("A CssSelector String") {
    it("should match id selector") {
      given("html")
      val html = <div>
				<span class="findClass">
					FoundClass
				</span>
				<span id="findId">FoundId</span>
      </div>
      when("selecting")
      val found = html.extract("#findId")
      then("right element is found")
      assert(found.head === <span id="findId">FoundId</span>)
    }
    it("should match id selector with child elements") {
    	given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<span id="findId"><h1>FoundId</h1></span>
    	</div>
    	when("selecting")
    	val found = html.extract("#findId")
    	then("right element is found")
    	assert(found.head === <span id="findId"><h1>FoundId</h1></span>)
    }
    it("should match id selector with child elements with spaces") {
    	given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<span id="findId">
    			<h1>FoundId</h1>
    		</span>
    	</div>
    	when("selecting")
    	val found = html.extract("#findId")
    	then("right element is found")
    	assert(found.head === <span id="findId">
    			<h1>FoundId</h1>
    		</span>)
    }
    it("should match class selector") {
    	given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<span id="findId">FoundId</span>
    	</div>
    	when("selecting")
    	val found = html.extract(".findClass")
    	then("right element is found")
    	assert(found.head === <span class="findClass">FoundClass</span>)
    	assert(found.size === 1)
    }
    it("should match class selector with child elements") {
    	given("html")
    	val html = <div>
    		<span class="findClass"><h1>FoundClass</h1></span>
    		<span id="findId"><h1>FoundId</h1></span>
    	</div>
    	when("selecting")
    	val found = html.extract(".findClass")
    	then("right element is found")
    	assert(found.head === <span class="findClass"><h1>FoundClass</h1></span>)
    }
    it("should match class selector with child elements with spaces") {
    	given("html")
    	val html = <div>
    		<span class="findClass">
    			<span>FoundClass</span>
    		</span>
    		<span id="findId">
    			<h1>FoundId</h1>
    		</span>
    	</div>
    	when("selecting")
    	val found = html.extract(".findClass")
    	then("right element is found")
    	assert(found.head === <span class="findClass">
    			<span>FoundClass</span>
    		</span>)
    }
    it("should match tag selector") {
    	given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<table><tr><td>TD</td></tr></table>
    		<span id="findId">FoundId</span>
    	</div>
    	when("selecting")
    	val found = html.extract("table")
    	then("right element is found")
    	assert(found.head === <table><tr><td>TD</td></tr></table>)
    }
    it("should match an attribute selector") {
    	given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<table><tr><td data-att="findMe">TD</td></tr></table>
    		<span id="findId">FoundId</span>
    	</div>
    	when("selecting")
    	val found = html.extract("[data-att=findMe]")
    	then("right element is found")
    	assert(found.head === <td data-att="findMe">TD</td>)
    }
    it("should match an element attribute selector") {
    	given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<table><tr><td data-att="findMe">TD</td></tr></table>
    		<span id="findId">FoundId</span>
    	</div>
    	when("selecting")
    	val found = html.extract("td[data-att=findMe]")
    	then("right element is found")
    	assert(found.head === <td data-att="findMe">TD</td>)
    	assert(found.size === 1)
    }
  }
  describe("A CssTransformer") {
  	it("should transform a node") {
    	given("html")
    	val html = <div><span id="findId">FoundId</span></div>
    	when("transforming")
    	val transformed = ("#findId" #> <span>Transformed</span>).apply(html) 
    	then("right element is transformed")
    	assert(transformed.toString === (<div><span>Transformed</span></div>).toString)
    }
  	it("should transform a nodeseq") {
  		given("html")
  		val html = <div><span id="findId"><span>FoundId</span><span>Can</span></span></div>
  		when("transforming")
  		val transformed = ("#findId" #> <span>Transformed<div>happy</div></span>).apply(html) 
  		then("right element is transformed")
  		assert(transformed.toString === (<div><span>Transformed<div>happy</div></span></div>).toString)
  	}
  	it("should transform a nested transform") {
  		given("html")
  		val html = <div><span id="findId"><span>FoundId</span></span></div>
  		when("transforming")
  		val transformed = ("#findId" #> ("span" #> <span>Transformed</span>)).apply(html) 
  		then("right element is transformed")
  		assert(transformed.toString === (<div><span id="findId"><span>Transformed</span></span></div>).toString)
  	}
  	it("should set into to selector") {
  		given("html")
  		val html = <div id="findId">FoundId</div>
  		when("transforming")
  		val transformed = ("#findId" #>> <span>Inserted</span>).apply(html) 
  		then("right element is transformed")
  		assert(transformed.toString === (<div id="findId"><span>Inserted</span></div>).toString)
  	}
  	it("should add to selector") {
  		given("html")
  		val html = <div id="findId"><span>Existing</span></div>
  		when("transforming")
  		val transformed = ("#findId" #+> <span>Added</span>).apply(html) 
  		then("right element is transformed")
  		assert(transformed.toString === (<div id="findId"><span>Existing</span><span>Added</span></div>).toString)
  	}
  	it("should transform list transform") {
  		given("html")
  		val html = <div><span class="find"><span>FoundId</span></span></div>
  		when("transforming")
  		val list = List("a", "b", "c")
  		val transformed = (".find" #> list.map(i => "span" #> <span>{i}</span>)).apply(html) 
  		then("right element is transformed")
  		assert(transformed.toString === (<div><span class="find"><span>a</span><span>b</span><span>c</span></span></div>).toString)
  	}
  	
  	it("should insert attibute") {
  		given("html")
  		val html = <div><span class="find">FoundId</span></div>
  		when("transforming")
  		val list = List("a", "b", "c")
  		val transformed = (".find" #@> Map("id" -> "inserted")).apply(html) 
  		then("right attributes is inserted")
  		assert(transformed.toString === (<div><span id="inserted" class="find">FoundId</span></div>).toString)
  	}
  }
}