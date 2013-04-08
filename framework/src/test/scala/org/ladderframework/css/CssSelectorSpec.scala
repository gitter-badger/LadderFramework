package org.ladderframework.css

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import CssSelector._
import scala.xml.Text
import scala.concurrent.Future
import scala.concurrent.Await

class CssSelectorSpec extends FunSpec with GivenWhenThen {

  describe("A CssSelector String") {
    it("should match id selector") {
      Given("html")
      val html = <div>
				<span class="findClass">
					FoundClass
				</span>
				<span id="findId">FoundId</span>
      </div>
      When("selecting")
      val found = html.extract("#findId")
      Then("right element is found")
      assert(found.head === <span id="findId">FoundId</span>)
    }
    it("should match id selector with child elements") {
    	Given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<span id="findId"><h1>FoundId</h1></span>
    	</div>
    	When("selecting")
    	val found = html.extract("#findId")
    	Then("right element is found")
    	assert(found.head === <span id="findId"><h1>FoundId</h1></span>)
    }
    it("should match id selector with child elements with spaces") {
    	Given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<span id="findId">
    			<h1>FoundId</h1>
    		</span>
    	</div>
    	When("selecting")
    	val found = html.extract("#findId")
    	Then("right element is found")
    	assert(found.head === <span id="findId">
    			<h1>FoundId</h1>
    		</span>)
    }
    it("should match class selector") {
    	Given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<span id="findId">FoundId</span>
    	</div>
    	When("selecting")
    	val found = html.extract(".findClass")
    	Then("right element is found")
    	assert(found.head === <span class="findClass">FoundClass</span>)
    	assert(found.size === 1)
    }
    it("should match class selector with child elements") {
    	Given("html")
    	val html = <div>
    		<span class="findClass"><h1>FoundClass</h1></span>
    		<span id="findId"><h1>FoundId</h1></span>
    	</div>
    	When("selecting")
    	val found = html.extract(".findClass")
    	Then("right element is found")
    	assert(found.head === <span class="findClass"><h1>FoundClass</h1></span>)
    }
    it("should match class selector with child elements with spaces") {
    	Given("html")
    	val html = <div>
    		<span class="findClass">
    			<span>FoundClass</span>
    		</span>
    		<span id="findId">
    			<h1>FoundId</h1>
    		</span>
    	</div>
    	When("selecting")
    	val found = html.extract(".findClass")
    	Then("right element is found")
    	assert(found.head === <span class="findClass">
    			<span>FoundClass</span>
    		</span>)
    }
    it("should match tag selector") {
    	Given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<table><tr><td>TD</td></tr></table>
    		<span id="findId">FoundId</span>
    	</div>
    	When("selecting")
    	val found = html.extract("table")
    	Then("right element is found")
    	assert(found.head === <table><tr><td>TD</td></tr></table>)
    }
    it("should match an attribute selector") {
    	Given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<table><tr><td data-att="findMe">TD</td></tr></table>
    		<span id="findId">FoundId</span>
    	</div>
    	When("selecting")
    	val found = html.extract("[data-att=findMe]")
    	Then("right element is found")
    	assert(found.head === <td data-att="findMe">TD</td>)
    }
    it("should match an element attribute selector") {
    	Given("html")
    	val html = <div>
    		<span class="findClass">FoundClass</span>
    		<table><tr><td data-att="findMe">TD</td></tr></table>
    		<span id="findId">FoundId</span>
    	</div>
    	When("selecting")
    	val found = html.extract("td[data-att=findMe]")
    	Then("right element is found")
    	assert(found.head === <td data-att="findMe">TD</td>)
    	assert(found.size === 1)
    }
  }
  describe("A CssTransformer") {
  	it("should transform a node") {
    	Given("html")
    	val html = <div><span id="findId">FoundId</span></div>
    	When("transforming")
    	val transformed = ("#findId" #> <span>Transformed</span>).apply(html) 
    	Then("right element is transformed")
    	assert(transformed.toString === (<div><span>Transformed</span></div>).toString)
    }
  	it("should transform a nodeseq") {
  		Given("html")
  		val html = <div><span id="findId"><span>FoundId</span><span>Can</span></span></div>
  		When("transforming")
  		val transformed = ("#findId" #> <span>Transformed<div>happy</div></span>).apply(html) 
  		Then("right element is transformed")
  		assert(transformed.toString === (<div><span>Transformed<div>happy</div></span></div>).toString)
  	}
  	it("should transform a nested transform") {
  		Given("html")
  		val html = <div><span id="findId"><span>FoundId</span></span></div>
  		When("transforming")
  		val transformed = ("#findId" #> ("span" #> <span>Transformed</span>)).apply(html) 
  		Then("right element is transformed")
  		assert(transformed.toString === (<div><span id="findId"><span>Transformed</span></span></div>).toString)
  	}
  	it("should set into to selector") {
  		Given("html")
  		val html = <div id="findId">FoundId</div>
  		When("transforming")
  		val transformed = ("#findId" #>> <span>Inserted</span>).apply(html) 
  		Then("right element is transformed")
  		assert(transformed.toString === (<div id="findId"><span>Inserted</span></div>).toString)
  	}
  	it("should add to selector") {
  		Given("html")
  		val html = <div id="findId"><span>Existing</span></div>
  		When("transforming")
  		val transformed = ("#findId" #+> <span>Added</span>).apply(html) 
  		Then("right element is transformed")
  		assert(transformed.toString === (<div id="findId"><span>Existing</span><span>Added</span></div>).toString)
  	}
  	it("should transform list transform") {
  		Given("html")
  		val html = <div><span class="find"><span>FoundId</span></span></div>
  		When("transforming")
  		val list = List("a", "b", "c")
  		val transformed = (".find" #> list.map(i => "span" #> <span>{i}</span>)).apply(html) 
  		Then("right element is transformed")
  		assert(transformed.toString === (<div><span>a</span><span>b</span><span>c</span></div>).toString)
  	}
  	
  	it("should insert attibute") {
  		Given("html")
  		val html = <div><span class="find">FoundId</span></div>
  		When("transforming")
  		val list = List("a", "b", "c")
  		val transformed = (".find" #@> Map("id" -> "inserted")).apply(html) 
  		Then("right attributes is inserted")
  		assert(transformed.toString === (<div><span id="inserted" class="find">FoundId</span></div>).toString)
  	}
  }
  
  describe("A Nested CssTransformation") {
  	it("should transform larger graph") {
  		Given("html")
  		val html = <div id="content">
  			<header class="jumbotron subhead" id="overview"><div class="container super-unit"><h1>Search</h1>
		  		<form class="form-search" action="search" method="GET">
						<div class="input-prepend"><button type="submit" class="btn">Search for</button><input type="text" name="query" class="span3 search-query input-large" placeholder="for artifacts, groups, etc..."/></div>
		  		</form>
					<p>Examples: <a href="search?query=hibernate">hibernate</a>, <a href="search?query=spring">spring</a>, <a href="search?query=logging">logging</a></p> 
  			</div></header>
  			<div class="container"><h2>Results</h2>
  				<div class="row">
  					<div class="span3"><h3>Filter</h3>
  						<table class="table-condensed l_group_facet">
		    				<thead><tr><th>Group</th></tr></thead><tbody><tr class="l_results"><td class="l_group"><a href="" class="l_group">@result.group</a></td></tr></tbody>
		    			</table>
  						<table class="table-condensed l_artifact_facet">
		    				<thead><tr><th>Artifact</th></tr></thead><tbody><tr class="l_results"><td class="l_artifact"><a href="" class="l_artifact">@result.artifact</a></td></tr></tbody>
		    			</table>
  					</div>
  					<div class="span9"><h3>Number of results (including versions) <span class="l_numberOfResults">XX</span></h3>
  						<table class="table table-striped table-hover table-condensed l_list_results">
		    				<thead><tr><th>Group</th><th>Artifact</th><th>Version</th><th>Name</th></tr></thead>
		    				<tbody><tr class="l_results l">
		    					<td class="l_group"><a href="" class="l_group">@result.group</a></td>
		    					<td class="l_artifact"><a href="" class="l_artifact">@result.artifact</a></td>
		    					<td class="l_version"><a href="" class="l_version">@result.version</a></td>
		    					<td class="l_name">@result.name</td>
		    				</tr></tbody>
		    			</table>
  					</div>
  				</div>
  			</div>
  		</div>
  		When("transforming")
  		
  		case class Result(group: String, artifactId: String, version: String, name: Option[String])
  		case class Alternative(name: String, count: Int)
  		case class Facet(name: String, alternatives: Seq[Alternative])
  		
  		val query = Option("")
  		
  		val results = (0 to 10000).map(i => Result("g" + i, "a" + i, "v" + i, Some("name is great" + i)))
  		val numberOfHits = 1000
  		val groupFacet = Facet(name = "group", alternatives = (0 to 10000).map(i => Alternative("a" + i, 1000 - i)))
  		val artifactFacet = Facet(name = "artifact", alternatives = (0 to 10000).map(i => Alternative("a" + i, 1000 - i)))
  		val versionFacet = Facet(name = "version", alternatives = (0 to 10000).map(i => Alternative("a" + i, 1000 - i)))
			val transformfunc = {
  			".l_group_facet" #> {
					".l_results" #> groupFacet.alternatives.map( alt => {
						"a" #@> Map("href" -> ("/artifact/" + alt.name)) &
						"a" #>> (alt.name + " (" + alt.count + ")")
					})
				} &
				".l_artifact_facet" #> {
					".l_results" #> artifactFacet.alternatives.map( alt => {
						"a" #@> Map("href" -> ("/artifact/" + alt.name)) &
						"a" #>> (alt.name + " (" + alt.count + ")")
					})
				} &
				".l_version_facet" #> {
					".l_results" #> versionFacet.alternatives.map( alt => {
						"a" #@> Map("href" -> ("/artifact/" + alt.name)) &
						"a" #>> (alt.name + " (" + alt.count + ")")
					})
				} &
				".l_numberOfResults" #> numberOfHits.toString &
				"[name=query]" #@> Map("value" -> query.getOrElse("")) &
				".l_list_results" #> {
					".l_results" #> results.map(result => {
						".l_group" #> {
							"a" #@> Map("href" -> ("/artifact/" + result.group)) &
							"a" #>> result.group
						} &
						".l_artifact" #> {
							"a" #@> Map("href" -> ("/artifact/" + result.group + "/" + result.artifactId)) &
							"a" #>> result.artifactId
						} &
						".l_version" #> {
							"a" #@> Map("href" -> ("/artifact/" + result.group + "/" + result.artifactId + "/" + result.version)) &
							"a" #>> result.version
						} &
						".l_name" #>> Text(result.name.getOrElse(""))
					})
				}
  		}
  		import scala.concurrent.ExecutionContext.Implicits.global
  		import scala.concurrent.duration._
  		val transform = Future({
  			val s = java.lang.System.currentTimeMillis()
  			val t = (transformfunc).apply(html)
  			println("time: " + (java.lang.System.currentTimeMillis() - s))
  			t
  		}) 
  		Then("it is done in a heart beat")
  		assert(Await.ready(transform, 10 second).isCompleted)
  	}
  }
}